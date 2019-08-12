#!/bin/bash

#SBATCH --job-name=runModelComp
#SBATCH --error=/scratch/users/aminaly/runModelComp.err
#SBATCH --output=/scratch/users/aminaly/runModelComp.out
#SBATCH --nodes=1
#SBATCH --time=02:00:00
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=12GB
#SBATCH --mail-type=END,FAIL 
#SBATCH --mail-user=aminaly@stanford.edu
#SBATCH -p diffenbaughmodel


module load R/3.4.0

cd $OAK/group_members/aminaly/precip-price
Rscript ./model_selection.R
