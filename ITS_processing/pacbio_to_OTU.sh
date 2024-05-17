#!/bin/bash

########################################
# Script for ITS Pacbio sequence processing
# Takes a folder of ITS sequences as input
#	0. Quality filtering
#	1. Primer trimming
#	2. ITS extraction
#	3. Chimera removal
#	4. Clustering, open-reference
#----------------------------------------------------------------------
# ARG PASSING

set -e

# default parameters
THREADS=1

usage() {
    echo -e "Usage:\n -f folder of only sequences in fq.gz -t threads to use"
}

# if no args, abort with help
[ $# -eq 0 ] && usage && exit 1

while getopts ":f:t:" arg; do
  case $arg in
    f) # number of threads
      FOLDER=${OPTARG}
      ;;
    t) # strain name
      THREADS=${OPTARG}
      ;;
    h) # help
      usage
      exit 0
      ;;
    *) # help then error
      usage
      exit 1
      ;;
  esac
done

if [ -z $FOLDER ] ; then
	usage
	exit 1
fi

#----------------------------------------------------------------------
# 0. Quality filtering
# Remove reads with more than 1 ambigious base and 2 expected errors

if [ ! -d tmpTRIM ]; then
	mkdir tmpTRIM
fi

for FILE in $FOLDER/*fq.gz; do
	# get the basename of the inputfile
	BASE=$(basename "$FILE" | cut -d. -f1)
	echo "Filtering $BASE on quality"


	zcat $FILE > tmpTRIM/$BASE.tmp.fq # python-scripts takes fastq as input

	python3 python_scripts/qual_filter_reads.py tmpTRIM/$BASE.tmp.fq tmpTRIM/$BASE.qual.filt.fq -m 1 -e 2
	gzip -f tmpTRIM/$BASE.qual.filt.fq

	rm tmpTRIM/$BASE.tmp.fq
done


#----------------------------------------------------------------------
# 1. Primer Trimming
# use --revcomp to normalize read-direction
for FILT in tmpTRIM/*.qual.filt.fq.gz; do
	BASE=$(basename "$FILT" | cut -d. -f1)
	echo "Trimming $BASE for primers"
	# Remove primers (F in forward orientation and R in rc orientation)
	cutadapt \
	--revcomp \
	-j $THREADS \
	-n 2 \
	-g GTACACACCGCCCGTCG \
	-a GCATATHANTAAGSGSAGGCG \
	-o tmpTRIM/$BASE.trim.fq \
	$FILT \
	> tmpTRIM/$BASE.trim.log

	gzip -f tmpTRIM/$BASE.trim.fq
done
#----------------------------------------------------------------------
# 2. ITS Extraction
# Extract ITS for each type of region
for region in "ALL" "ITS1" "ITS2" ; do

	# Create folder for results if missing 
	if [ ! -d $region ]; then
		mkdir $region
	fi

	for TRIMMED in tmpTRIM/*.trim.fq.gz; do
		BASE=$(basename "$TRIMMED" | cut -d. -f1)

	echo "Extracting $region from $BASE"
		itsxpress\
		--fastq $TRIMMED \
		--single_end \
		--region $region \
		--log $region/$BASE.$region.log \
		--threads $THREADS \
		--outfile $region/$BASE.$region.fq 
	done
done

# clean up trimmed seq
cat tmpTRIM/*.trim.log > $FOLDER.trim.log.txt
rm tmpTRIM -r

#----------------------------------------------------------------------
# 3. Chimera removal

# Remove chimeras from each sample
for region in "ALL" "ITS1" "ITS2" ; do

	for SAMPLE in $region/*$region.fq; do

		BASE=$(basename "$SAMPLE" | cut -d. -f1)
		echo "Dereplicating $BASE"
		vsearch \
		--derep_fulllength $SAMPLE \
		--fasta_width 0\
		--threads $THREADS \
		--sizeout \
		--output - \
		2> $region/$BASE.derep.log \
		| gzip > $region/$BASE.derep.fa.gz

		echo "Removing chimeras for $region from $BASE"

		# Generate list of nonchimeras to remove keep
		vsearch \
		--uchime_ref $region/$BASE.derep.fa.gz \
		--db ../DB/uchime_reference_dataset_16_10_2022.fasta \
		--fasta_width 0 \
		--sizein --sizeout \
		--threads $THREADS \
		--chimeras - \
		--nonchimeras $region/$BASE.$region.nonchimera.fasta \
		> $region/$BASE.chimera.log 

		# Extract nonchimeras from original fastq-file
		awk 'NR%2==0' $region/$BASE.$region.nonchimera.fasta > $region/$BASE.$region.tmp.fasta
		# Extract sequence by matching reads from original reads to sequence from dereplicated
		# nonchimeras
		grep --no-group-separator -A 2 -B 1 -F -f $region/$BASE.$region.tmp.fasta $SAMPLE \
		| gzip > $region/$BASE.$region.nonchimera.fq.gz

		## Clean-up on the go:
		rm $region/$BASE.$region.tmp.fasta
		echo $SAMPLE
		rm $SAMPLE
		rm $region/$BASE.$region.nonchimera.fasta
		rm $region/$BASE.chimera.log
	done
done



#----------------------------------------------------------------------
# 4. Clustering, open-reference

# Concatanate nonchimeric ITS regions and dereplicate
for region in "ALL" "ITS1" "ITS2"; do
	# collect all samples
	cat $region/*nonchimera.fq.gz > $region.fq.gz

	echo "dereplicating $region samples"
	# Dereplicate all samples
	vsearch \
        --derep_fulllength $region.fq.gz \
        --fasta_width 0\
        --threads $THREADS \
        --sizeout \
        --output - \
        2> $region.derep.log \
	| gzip > $region.derep.fa.gz

	# sort by abundance
	vsearch --sortbysize $region.derep.fa.gz --output $region.derep.sorted.fa
	gzip $region.derep.sorted.fa.gz
done

#concat all dereplicated files with the UNITE DB
# Here order is important
cat UNITE.final.derep.fulllength.fa.gz ALL.derep.sorted.fa.gz ITS1.derep.sorted.fa.gz ITS2.derep.sorted.fa.gz > reads.for.clustering.fa.gz

# Cluster files
vsearch \
--cluster_smallmem reads.for.clustering.fa.gz \
--id 0.98 \
--iddef 2 \
--sizein --sizeout \
--usersort \
--relabel_sha1 \
--threads $THREADS \
--strand both \
--centroids clust.centroids.fasta \
--consout clust.consensus.fasta \
--uc clust.mapfile.uc


# Dereplicate and label each fasta header in file
for FILE in $FOLDER/*fq.gz; do
       # get the basename of the inputfile

        BASE=$(basename "$FILE" | cut -d. -f1)
        echo "Preparing $BASE for mapping"

        cat ALL/$BASE*nonchimera.fq.gz ITS1/$BASE*nonchimera.fq.gz ITS2/$BASE*nonchimera.fq.gz \
	> $BASE.tmp.col.fq.gz

	# Dereplicate all samples
        vsearch \
        --derep_fulllength $BASE.tmp.col.fq.gz \
        --fasta_width 0\
        --threads $THREADS \
        --sizeout \
        --output - \
        | sed  "s/^>/>sample=\"$BASE\" /g" \
        | gzip > $BASE.derep.map.fa.gz

	rm $BASE.tmp.col.fq.gz
done

cat *derep.map.fa.gz > all.reads.map.fa.gz


# Map reads from each sample onto OTUs
vsearch \
  --usearch_global all.reads.map.fa.gz \
  --db clust.centroids.fasta  \
  --id 0.98 \
  --strand both \
  --qmask none \
  --dbmask none \
  --sizein --sizeout \
  --fasta_width 0 \
  --otutabout OTU.ITS.txt \
  --threads $THREADS

# clean-up
rm *derep.map.fa.gz
rm ALL ITS1 ITS2 -r

# rmv short OTUs (<250 nuc)
seqkit seq --max-len 249 clust.centroids.fasta \
| grep ">" \
| cut -d ";" -f1 \
| sed 's/^>//' \
 > OTU.short.to.remove.txt

grep -v -E -f OTU.short.to.remove.txt OTU.ITS.txt > OTU.ITS.filt.txt
rm OTU.short.to.remove.txt
rm OTU.ITS.txt

