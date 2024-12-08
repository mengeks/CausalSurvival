#!/bin/bash
#SBATCH -J gen_data
#SBATCH -o scripts/datagen/slurm-logs/gendata%A_%a.out
#SBATCH -p sapphire
#SBATCH -c 1
#SBATCH --array=1-200
#SBATCH --mem=1G
#SBATCH -t 08:30:00

my_packages=${HOME}/R/ifxrstudio/RELEASE_3_18
rstudio_singularity_image="/n/singularity_images/informatics/ifxrstudio/ifxrstudio:RELEASE_3_18.sif"

PARAMS_FILE=$1
VERBOSE=$(jq -r '.verbose' $PARAMS_FILE)
R=$(jq -r '.R' $PARAMS_FILE)

i=$SLURM_ARRAY_TASK_ID

for N in $(jq -r '.n_list[]' $PARAMS_FILE); do
  for ETA_TYPE in $(jq -r '.eta_type_list[]' $PARAMS_FILE); do
    for HTE_TYPE in $(jq -r '.HTE_type_list[]' $PARAMS_FILE); do

      LOG_FILE="/n/holylabs/LABS/pillai_lab/Users/xmeng1/CausalSurvival/scripts/datagen/R-code-logs/log_n${N}_eta${ETA_TYPE}_HTE${HTE_TYPE}_iter${i}.log"

      [ "$VERBOSE" -eq 2 ] && echo "Running: n=$N, eta_type=$ETA_TYPE, HTE_type=$HTE_TYPE, iteration=$i"

      singularity_command="singularity exec --cleanenv --env R_LIBS_USER=${my_packages} ${rstudio_singularity_image}"
      $singularity_command Rscript --vanilla ./scripts/datagen/simulate_data.R \
        --i "$i" \
        --n "$N" \
        --eta_type "$ETA_TYPE" \
        --HTE_type "$HTE_TYPE" \
        --params_file "$PARAMS_FILE" \
        --verbose "$VERBOSE" > "$LOG_FILE" 2>&1

      [ "$VERBOSE" -eq 2 ] && echo "Completed: n=$N, eta_type=$ETA_TYPE, HTE_type=$HTE_TYPE, iteration=$i. Log: $LOG_FILE"
      
    done
  done
done
