#!/bin/bash

# Read in the JSON parameters file
PARAMS_FILE=$1
VERBOSE=$(jq -r '.verbose' $PARAMS_FILE)

# Iterate over the combinations of parameters
for N in $(jq -r '.n_list[]' $PARAMS_FILE); do
  for ETA_TYPE in $(jq -r '.eta_type_list[]' $PARAMS_FILE); do
    for CATE_TYPE in $(jq -r '.CATE_type_list[]' $PARAMS_FILE); do

      # Print parameters if verbosity is 2
      if [ "$VERBOSE" -eq 2 ]; then
        echo "n: $N   eta_type: $ETA_TYPE   CATE_type: $CATE_TYPE"
      fi

      # Loop through R iterations
      R=$(jq -r '.R' $PARAMS_FILE)
      for (( i=1; i<=$R; i++ )); do
        # Call the R script with the current parameters
        Rscript --vanilla simulate_data.R \
          --i $i \
          --n $N \
          --eta_type $ETA_TYPE \
          --CATE_type $CATE_TYPE \
          --params_file $PARAMS_FILE \
          --verbose $VERBOSE
        
        if [ "$VERBOSE" -eq 2 ]; then
          echo "Iteration $i completed."
        fi
      done
    done
  done
done
