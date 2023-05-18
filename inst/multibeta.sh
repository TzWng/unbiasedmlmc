#!/bin/bash
#SBATCH --partition=main           # Partition (job queue)
#SBATCH --job-name=unmlmc          # Assign an short name to your job
#SBATCH --nodes=1                  # Number of nodes you require
#SBATCH --ntasks=1                 # Total # of tasks across all nodes
#SBATCH --cpus-per-task=1          # Cores per task (>1 if multithread tasks)
#SBATCH --mem=1600                 # Real memory (RAM) required (MB)
#SBATCH --time=24:00:00            # Total run time limit (HH:MM:SS)
#SBATCH --export=ALL               # Export you current env to the job env

cd /home/tw522/Desktop/unmlmc/code
#module purge
#module load intel/17.0.4 R-Project/3.4.1
#export R_LIBS=~/my.R-3.4.1
run=$1
Rscript --no-save run.batch.multibeta.R > ${run}.Rout