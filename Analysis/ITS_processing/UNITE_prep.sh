#!/bin/bash
########################################
# Script for ITS region extraction from UNITE DB
# takes a .fasta.gz compressed file as input and
# the number of threads to use
########################################

UNITE = $1
THREADS = $2

# Unpack file
zcat $UNITE > $UNITE.fasta

# ITS Extraction
# Note: This takes quite a while
# If space is laggin, set --graphical F
ITSx \
  -i $UNITE.fasta \
  --complement T \
  --save_regions all \
  --positions T \
  -t all \
  --cpu $THREADS \
  --preserve T \
  -o ITSX_UNITE
