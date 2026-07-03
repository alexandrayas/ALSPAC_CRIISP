#!/bin/bash

#SBATCH --job-name=mi_multd
#SBATCH --account=account_no
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --time=100:00:00
#SBATCH --array=1-100

# Change to the working directory
cd /user/work/username

#Note: need to install packages locally

# Load the R module
module load languages/R/4.5.1

# Echo the current array task ID
echo "Running array task with imp_num=${SLURM_ARRAY_TASK_ID}"

# Run R script with the array task ID added to the seed mice argument
Rscript criisp_multd_mi_array.R ${SLURM_ARRAY_TASK_ID}

# Unload the R module
module unload languages/R/4.5.1
