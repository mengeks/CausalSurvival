#!/bin/bash

# Usage: ./run_experiment.sh path/to/config.json

json_file=$1

if [[ -z "$json_file" ]]; then
  echo "Usage: $0 path/to/config.json"
  exit 1
fi

# Extract R value from the JSON file (number of iterations)
R=$(jq '.R' $json_file)

for (( i=1; i<=R; i++ ))
do
  echo "Running iteration $i"
  Rscript -e "source('R/TV-CSL-runner.R'); run_experiment_iteration($i, '$json_file')" | tee -a "results/TV-CSL/log_iteration_${i}.txt"
done
