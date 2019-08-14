#!/bin/bash
#SBATCH --job-name=seasonalcalc
#SBATCH --nodes=1
#SBATCH --time=03:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=12GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh


module load R/3.4.0

cd $OAK/group_members/aminaly/precip-price
Rscript ./calc_precips.R
