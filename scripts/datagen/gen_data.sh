#!/bin/bash

# Load R and other necessary modules (if applicable)
# module load R  # Example if running on a cluster

# Read in the JSON parameters file
PARAMS_FILE=$1

# Iterate over the combinations of parameters
for N in $(jq -r '.n_list[]' $PARAMS_FILE); do
  for ETA_TYPE in $(jq -r '.eta_type_list[]' $PARAMS_FILE); do
    for CATE_TYPE in $(jq -r '.CATE_type_list[]' $PARAMS_FILE); do
      
      # Print parameters if verbosity is 2
      VERBOSE=$(jq -r '.verbose' $PARAMS_FILE)
      if [ $VERBOSE -eq 2 ]; then
        echo "n: $N   eta_type: $ETA_TYPE   CATE_type: $CATE_TYPE"
      fi

      # Prepare the output folder
      IS_TIME_VARYING=$(jq -r '.other_params.is_time_varying' $PARAMS_FILE)
      if [ "$IS_TIME_VARYING" = "true" ]; then
        FOLDER_NAME="data/simulated"
      else
        FOLDER_NAME="data/simulated/non-time-varying"
      fi
      OUTPUT_DIR="$FOLDER_NAME/${ETA_TYPE}_${CATE_TYPE}"
      mkdir -p $OUTPUT_DIR
      
      if [ $VERBOSE -eq 1 ]; then
        echo "Save path: $OUTPUT_DIR"
      fi
      
      # Loop through R iterations
      R=$(jq -r '.R' $PARAMS_FILE)
      for (( i=1; i<=$R; i++ )); do
        # Run the R script with the current parameters
        Rscript --vanilla simulate_data.R \
          --i $i \
          --n $N \
          --eta_type $ETA_TYPE \
          --CATE_type $CATE_TYPE \
          --output_dir $OUTPUT_DIR \
          --params_file $PARAMS_FILE \
          --verbose $VERBOSE
        
        if [ $VERBOSE -eq 2 ]; then
          echo "Iteration $i completed."
        fi
      done

    done
  done
done
