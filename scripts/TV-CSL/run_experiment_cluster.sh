#!/bin/bash
#SBATCH -J gen_data         # Job name
#SBATCH -o logs/gen_data_%j.out   # Standard output log (%j will be replaced with job ID)
#SBATCH -e logs/gen_data_%j.err   # Standard error log
#SBATCH -p sapphire           # Partition (queue) name
#SBATCH -c 100               # Number of cores
#SBATCH --mem=4000          # Memory in MB
#SBATCH -t 02:00:00         # Runtime (hours:minutes:seconds)

# Usage: ./run_experiment.sh path/to/config.json [verbose]

module load R/4.2.2-fasrc01

json_file=$1
verbose=${2:-0}  # Default verbosity is 0 if not provided

if [[ -z "$json_file" ]]; then
  echo "Usage: $0 path/to/config.json [verbose]"
  exit 1
fi

# Extract R value from the JSON file (number of iterations)
R=$(jq '.R' $json_file)

# log_dir="$HOME/Dropbox (Harvard University)/Xiang_Iav/CausalSurvival/results/TV-CSL"
log_dir="/n/holylabs/LABS/pillai_lab/Users/xmeng1/CausalSurvival/scripts/TV-CSL/results"
mkdir -p "$log_dir"

for (( i=1; i<=R; i++ ))
do
  echo "Running iteration $i with verbose level $verbose"
  Rscript -e "source('scripts/TV-CSL/TV-CSL-runner.R'); run_experiment_iteration($i, '$json_file', $verbose)" > "$log_dir/log_iteration_${i}.txt" 2>&1
done
