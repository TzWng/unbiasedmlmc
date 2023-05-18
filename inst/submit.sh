#!/bin/bash
#SBATCH --partition=main         # Partition (job queue)
#SBATCH --requeue                # Return job to the queue if preempted
#SBATCH --job-name=unmlmc        # Assign an short name to your job
#SBATCH --ntasks=1               # Total # of tasks across all nodes
#SBATCH --cpus-per-task=1        # Cores per task (>1 if multithread tasks)
#SBATCH --mem=1600               # Real memory (RAM) required (MB)
#SBATCH --time=24:00:00          # Total run time limit (HH:MM:SS)
#SBATCH --output=submit.out      # STDOUT output file
#SBATCH --export=ALL             # Export you current env to the job env
for run in $(seq 1 500)
do
	echo ${run}
	sbatch multibeta.sh ${run}
	while [ $(squeue -u tw522 | wc -l) -ge 500 ] 
	do
		sleep 15 #pause this submit program in 15 seconds
	done
done
