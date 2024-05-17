#!/bin/bash
#################################################
#Collect ITSx output in order from full to partial
# and dereplicate to prepare for OTU clustering
##################################################

# Generete TSV files
seqkit fx2tab *.full.fasta | sed 's/\t$//g' | csvtk add-header -t -n id,Full > 0.tmp.full.tsv
seqkit fx2tab *.ITS1.fasta | sed 's/\t$//g' | csvtk add-header -t -n id,ITS1 > 1.tmp.ITS1.tsv
seqkit fx2tab *.5_8S.fasta | sed 's/\t$//g' | csvtk add-header -t -n id,58S  > 1.tmp.58S.tsv
seqkit fx2tab *.ITS2.fasta | sed 's/\t$//g' | csvtk add-header -t -n id,ITS2 > 1.tmp.ITS2.tsv

# Join ITS fragments
csvtk join -t -f "id" 1.tmp.ITS1.tsv 1.tmp.58S.tsv 1.tmp.ITS2.tsv > 2.ITS1.58S.ITS2.tsv
csvtk join -t -f "id" 1.tmp.ITS1.tsv 1.tmp.58S.tsv > 3.ITS1.58S.tsv
csvtk join -t -f "id" 1.tmp.58S.tsv 1.tmp.ITS2.tsv > 4.58S.ITS2.tsv

# Format table 2-4 so they have just a ID and seq cols
awk -F'\t' 'BEGIN {OFS="\t"} {print $1, $2$3$4}' 2.ITS1.58S.ITS2.tsv > 2.format.ITS1.58S.ITS2.tsv
awk -F'\t' 'BEGIN {OFS="\t"} {print $1, $2$3$4}' 3.ITS1.58S.tsv > 3.format.ITS1.58S.tsv
awk -F'\t' 'BEGIN {OFS="\t"} {print $1, $2$3$4}' 4.58S.ITS2.tsv > 4.format.58S.ITS2.tsv

# Append

files_in_order=("2.format.ITS1.58S.ITS2.tsv" "3.format.ITS1.58S.tsv" "4.format.58S.ITS2.tsv" "1.tmp.ITS1.tsv" "1.tmp.58S.tsv" "1.tmp.ITS2.tsv")

# File to append
# Can´t be empty because that break the awk command
#cat 0.tmp.full.tsv > UNITE.final.tsv

for file in ${files_in_order[@]}; do

	echo "appending $file"
	awk 'NR==FNR{a[$1];next} !($1 in a)' UNITE.final.tsv $file > tmp.to.add.tsv
	cat tmp.to.add.tsv >> UNITE.final.tsv
done
 


# Convert table back to fasta and Ns
awk '{ print $1 "\t" $2 }' UNITE.final.tsv \
	| tail -n +2 \
	| seqkit tab2fx -w 0  \
	| seqkit replace -p "^n+|n+$" -r "" -is -w 0 \
	| gzip -6 > UNITE.final.fasta.gz

# Dereplicate database
vsearch --derep_fulllength ´
UNITE.final.fasta.gz \
--fasta_width 0 \
--threads 16 \ 
--sizeout 
--output - \
| gzip > UNITE.final.derep.fulllength.fasta.gz




# Clean-up
rm 0.tmp.full.tsv \
1.tmp.ITS1.tsv \
1.tmp.58S.tsv \
1.tmp.ITS2.tsv \
2.ITS1.58S.ITS2.tsv \
3.ITS1.58S.tsv \
4.58S.ITS2.tsv \
2.format.ITS1.58S.ITS2.tsv \
3.format.ITS1.58S.tsv \
4.format.58S.ITS2.tsv \
tmp.to.add.tsv \
UNITE.final.tsv



