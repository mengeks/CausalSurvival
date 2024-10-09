#!/bin/bash
#SBATCH -J fit_model         # Job name
#SBATCH -o scripts/TV-CSL/logs/fit_model_%j.out   # Standard output log (%j will be replaced with job ID)
#SBATCH -e scripts/TV-CSL/logs/fit_model_%j.err   # Standard error log
#SBATCH -p sapphire           # Partition (queue) name
#SBATCH -c 100               # Number of cores
#SBATCH --mem=32000          # Memory in MB
#SBATCH -t 02:00:00         # Runtime (hours:minutes:seconds)


# module load R/4.2.2-fasrc01
# export R_LIBS_USER=/n/home01/xmeng1/R/ifxrstudio/RELEASE_3_18

# Using singularity
my_packages=${HOME}/R/ifxrstudio/RELEASE_3_18
rstudio_singularity_image="/n/singularity_images/informatics/ifxrstudio/ifxrstudio:RELEASE_3_18.sif"

json_file=$1
verbose=${2:-0}

if [[ -z "$json_file" ]]; then
  echo "Usage: $0 path/to/config.json [verbose]"
  exit 1
fi

R=$(jq '.R' $json_file)

log_dir="/n/holylabs/LABS/pillai_lab/Users/xmeng1/CausalSurvival/scripts/TV-CSL/results"
mkdir -p "$log_dir"


export json_file verbose log_dir

singluarity_command="singularity exec --cleanenv --env R_LIBS_USER=${my_packages} ${rstudio_singularity_image}"

# seq 1 $R | parallel -j 100 --eta "echo 'Running iteration {} with verbose level $verbose'; Rscript -e 'source(\"scripts/TV-CSL/TV-CSL-runner.R\"); run_experiment_iteration({}, \"$json_file\", $verbose)' > $log_dir/log_iteration_{}.txt 2>&1"
seq 1 $R | parallel -j 100 --eta "echo 'Running iteration {} with verbose level $verbose'; $singluarity_command Rscript -e 'source(\"scripts/TV-CSL/TV-CSL-runner.R\"); run_experiment_iteration({}, \"$json_file\", $verbose)' > $log_dir/log_iteration_{}.txt 2>&1"
