#!/bin/bash
module load R
Rscript AnalyzeNYT.R -s $1 -m $2 -d $3 -r $4 -t $5 -b $6 -o $7
