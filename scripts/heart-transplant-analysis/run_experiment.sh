#!/bin/bash
#SBATCH -J fit_model         # Job name
#SBATCH -o scripts/heart-transplant-analysis/logs/fit_model_%A_%a.out   # output and error in one file
#SBATCH -p sapphire           # Partition (queue) name
#SBATCH -c 1              # Number of cores
#SBATCH --array=1-100  # Size of the array
#SBATCH --mem=8G          # Memory in GB
#SBATCH -t 01:00:00         # Runtime (hours:minutes:seconds)


my_packages=${HOME}/R/ifxrstudio/RELEASE_3_18
rstudio_singularity_image="/n/singularity_images/informatics/ifxrstudio/ifxrstudio:RELEASE_3_18.sif"

json_file=$1
verbose=${2:-0}


log_dir="/n/holylabs/LABS/pillai_lab/Users/xmeng1/CausalSurvival/scripts/heart-transplant-analysis/results"
mkdir -p "$log_dir"

export json_file verbose log_dir

singularity_command="singularity exec --cleanenv --env R_LIBS_USER=${my_packages} ${rstudio_singularity_image}"

echo "Running iteration $SLURM_ARRAY_TASK_ID with verbose level $verbose" # this goes to the .out
$singularity_command Rscript -e "source('scripts/heart-transplant-analysis/heart-transplant-sim-runner.R'); run_experiment_iteration(${SLURM_ARRAY_TASK_ID}, '$json_file', $verbose)" > "$log_dir/log_iteration_${SLURM_ARRAY_TASK_ID}.txt" 2>&1

