# -*- coding: utf-8 -*-
"""
Cread 2023-11-06
@author: dalofa

Filters a fastq-file based on expected number of errors and
ambigious bases found in a given read.
"""

import argparse
from Bio import SeqIO


def exp_errors(quality):
    '''Calculates the expected no. of errors given a quality score in phred-format'''
    e_sum=0.0
    for phred in quality:
        q = ord(phred)-33 # convert from phred to q
        error = 10 **(-q/10)
        e_sum=e_sum+error
    return e_sum
    
def filter_reads(input_file, output_file, max_ambiguous_bases, max_expected_errors):
    '''Filters reads based on maximum allowed numbers of ambious bases and expected errors'''
    with open(input_file, "r") as in_handle, open(output_file, "w") as out_handle:
        for record in SeqIO.parse(in_handle, "fastq"):
            seq = str(record.seq)
            quality = record.letter_annotations["phred_quality"]
            ambiguous_bases = sum(1 for base in seq if base not in "ACGTacgt")
            expected_errors = sum([10 ** (-q / 10) for q in quality])
    
            if ambiguous_bases <= max_ambiguous_bases and expected_errors <= max_expected_errors:
                SeqIO.write(record, out_handle, "fastq")
            
if __name__ == "__main__":
    # Customize the usage message to not display all capital letters
    custom_usage = "Usage: python filter_fastq.py input_file output_file -m MAX_AMBIGUOUS_BASES -e MAX_EXPECTED_ERRORS"

    parser = argparse.ArgumentParser(description="Filter reads based on ambiguous bases and expected errors.", usage=custom_usage)
    parser.add_argument("input_file", help="Input FASTQ file")
    parser.add_argument("output_file", help="Output FASTQ file")
    parser.add_argument("-m", "--max_ambiguous_bases", type=int, required=True, help="Maximum ambiguous bases allowed")
    parser.add_argument("-e", "--max_expected_errors", type=float, required=True, help="Maximum expected errors allowed")

    args = parser.parse_args()

    filter_reads(args.input_file, args.output_file, args.max_ambiguous_bases, args.max_expected_errors)