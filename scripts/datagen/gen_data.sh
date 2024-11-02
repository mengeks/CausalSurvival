#!/bin/bash

# Read in the JSON parameters file
PARAMS_FILE=$1
VERBOSE=$(jq -r '.verbose' $PARAMS_FILE)

# Define a function to call the R script for each parameter set and redirect output to a log file
run_simulation() {
  local i=$1
  local N=$2
  local ETA_TYPE=$3
  local HTE_TYPE=$4
  local PARAMS_FILE=$5
  local VERBOSE=$6

  # Create a unique log file for each run
  LOG_FILE="./logs/log_n${N}_eta${ETA_TYPE}_HTE${HTE_TYPE}_iter${i}.log"

  # If the log file already exists, skip this iteration
  if [ -f "$LOG_FILE" ]; then
    # if [ "$VERBOSE" -eq 2 ]; then
      echo "Log file for iteration $i already exists. Skipping."
    # fi
    return 0
  fi

  # Call the R script with the current parameters and redirect stdout and stderr to the log file
  Rscript --vanilla simulate_data.R \
    --i "$i" \
    --n "$N" \
    --eta_type "$ETA_TYPE" \
    --HTE_type "$HTE_TYPE" \
    --params_file "$PARAMS_FILE" \
    --verbose "$VERBOSE" > "$LOG_FILE" 2>&1

  if [ "$VERBOSE" -eq 2 ]; then
    echo "Iteration $i completed for n=$N, eta_type=$ETA_TYPE, HTE_type=$HTE_TYPE. Log: $LOG_FILE"
  fi
}

export -f run_simulation

# Iterate over the combinations of parameters and parallelize the iterations
for N in $(jq -r '.n_list[]' $PARAMS_FILE); do
  for ETA_TYPE in $(jq -r '.eta_type_list[]' $PARAMS_FILE); do
    for HTE_TYPE in $(jq -r '.HTE_type_list[]' $PARAMS_FILE); do
      
      # Print parameters if verbosity is 2
      if [ "$VERBOSE" -eq 2 ]; then
        echo "n: $N   eta_type: $ETA_TYPE   HTE_type: $HTE_TYPE"
      fi

      # Get the value of R
      R=$(jq -r '.R' $PARAMS_FILE)
      
      # Use GNU Parallel to parallelize the iterations and output to log files
      seq 1 $R | parallel -j 7 run_simulation {} $N $ETA_TYPE $HTE_TYPE $PARAMS_FILE $VERBOSE

    done
  done
done
