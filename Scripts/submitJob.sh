#!/bin/bash
module load R
source ~/.bash_profile
Rscript AnalyzeNYT.R -s $1 -m $2 -a $3 -d $4 -r $5 -t $6 -b $7 -o $8
