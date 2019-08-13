#!/bin/bash
#SBATCH --job-name=seasonalcalc
#SBATCH --error=/scratch/users/aminaly/seasonalcalc.err
#SBATCH --output=/scratch/users/aminaly/seasonalcalc.out
#SBATCH --nodes=1
#SBATCH --time=01:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=12GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaugh


module load R/3.4.0

cd $OAK/group_members/aminaly/precip-price
Rscript ./calc_precips.R
