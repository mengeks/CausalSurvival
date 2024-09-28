#!/bin/bash

# Usage: ./run_experiment.sh path/to/config.json [verbose]

json_file=$1
verbose=${2:-0}  # Default verbosity is 0 if not provided

if [[ -z "$json_file" ]]; then
  echo "Usage: $0 path/to/config.json [verbose]"
  exit 1
fi

# Extract R value from the JSON file (number of iterations)
R=$(jq '.R' $json_file)

for (( i=1; i<=R; i++ ))
do
  echo "Running iteration $i with verbose level $verbose"
  
  # Redirect both stdout and stderr to the log file
  Rscript -e "source('scripts/TV-CSL/TV-CSL-runner.R'); run_experiment_iteration($i, '$json_file', $verbose)" > "results/TV-CSL/log_iteration_${i}.txt" 2>&1
done
