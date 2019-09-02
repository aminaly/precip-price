#!/bin/bash
#SBATCH --job-name=calc_precips
#SBATCH --nodes=1
#SBATCH --array=1-6
#SBATCH --time=05:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=60GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh


module load R/3.4.0

cd $OAK/group_members/aminaly/precip-price
let buffer=$SLURM_ARRAY_TASK_ID
Rscript ./calc_precips.R $buffer 
