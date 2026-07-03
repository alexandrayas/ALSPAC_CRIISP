#!/bin/bash

#SBATCH --job-name=pain
#SBATCH --account=account_no
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --time=40:00:00
#SBATCH --array=1-15

# Change to the working directory
cd /user/work/username

# Point Stata to local libncurses copy
export LD_LIBRARY_PATH=/user/work/username/libncurses:$LD_LIBRARY_PATH

# Load the Stata module
module load apps/stata/17

# Echo the current array task ID
echo "Running array task with modn=${SLURM_ARRAY_TASK_ID}"

# Run Stata with the array task ID as the modn argument
stata -b do '"5ci. gformula_hpc_wpain.do"' ${SLURM_ARRAY_TASK_ID}

# Unload the Stata module
module unload apps/stata/17
