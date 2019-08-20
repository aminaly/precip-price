#!/bin/bash
#SBATCH --job-name=bootstrapcalcsbuf.25
#SBATCH --nodes=1
#SBATCH --time=03:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=60GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh


module load R/3.4.0

cd $OAK/group_members/aminaly/precip-price

Rscript ./bootstrap_data.R 
