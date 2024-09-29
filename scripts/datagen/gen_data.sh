#!/bin/bash

# Read in the JSON parameters file
PARAMS_FILE=$1
VERBOSE=$(jq -r '.verbose' $PARAMS_FILE)

# Define a function to call the R script for each parameter set
run_simulation() {
  local i=$1
  local N=$2
  local ETA_TYPE=$3
  local CATE_TYPE=$4
  local PARAMS_FILE=$5
  local VERBOSE=$6

  # Call the R script with the current parameters
  Rscript --vanilla simulate_data.R \
    --i "$i" \
    --n "$N" \
    --eta_type "$ETA_TYPE" \
    --CATE_type "$CATE_TYPE" \
    --params_file "$PARAMS_FILE" \
    --verbose "$VERBOSE"

  if [ "$VERBOSE" -eq 2 ]; then
    echo "Iteration $i completed for n=$N, eta_type=$ETA_TYPE, CATE_type=$CATE_TYPE."
  fi
}

export -f run_simulation

# Iterate over the combinations of parameters and parallelize the iterations
for N in $(jq -r '.n_list[]' $PARAMS_FILE); do
  for ETA_TYPE in $(jq -r '.eta_type_list[]' $PARAMS_FILE); do
    for CATE_TYPE in $(jq -r '.CATE_type_list[]' $PARAMS_FILE); do
      
      # Print parameters if verbosity is 2
      if [ "$VERBOSE" -eq 2 ]; then
        echo "n: $N   eta_type: $ETA_TYPE   CATE_type: $CATE_TYPE"
      fi

      # Get the value of R
      R=$(jq -r '.R' $PARAMS_FILE)
      
      # Use GNU Parallel to parallelize the iterations
      seq 1 $R | parallel -j 7 run_simulation {} $N $ETA_TYPE $CATE_TYPE $PARAMS_FILE $VERBOSE

    done
  done
done
