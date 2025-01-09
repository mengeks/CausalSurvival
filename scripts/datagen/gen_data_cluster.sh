#!/bin/bash
#SBATCH -J gen_data
#SBATCH -o scripts/datagen/slurm-logs/gendata%A_%a.out
#SBATCH -p sapphire
#SBATCH -c 1
#SBATCH --array=1-110
#SBATCH --mem=1G
#SBATCH -t 08:30:00

my_packages=${HOME}/R/ifxrstudio/RELEASE_3_18
rstudio_singularity_image="/n/singularity_images/informatics/ifxrstudio/ifxrstudio:RELEASE_3_18.sif"

PARAMS_FILE=$1
VERBOSE=$(jq -r '.verbose' $PARAMS_FILE)

i=$SLURM_ARRAY_TASK_ID


declare -a eta_types=("linear" "non-linear")
declare -a hte_types=("zero" "non-linear" "linear")
declare -a tx_difficulties=("complex" "constant")


# declare -A dgp_map=(
#   ["linear_zero_complex"]="DGP 2.1"
#   ["non-linear_zero_complex"]="DGP 2.2"
#   ["non-linear_non-linear_constant"]="DGP 3"
#   ["non-linear_non-linear_complex"]="DGP 4"
#   ["non-linear_linear_complex"]="DGP 5"
# )

declare -A dgp_map=(
  ["non-linear_linear_complex"]="DGP 5"
)

# Loop through parameter combinations
for ETA_TYPE in "${eta_types[@]}"; do
  for HTE_TYPE in "${hte_types[@]}"; do
    for tx_difficulty in "${tx_difficulties[@]}"; do
      # Validate combination
      if [[ ! "${dgp_map["${ETA_TYPE}_${HTE_TYPE}_${tx_difficulty}"]}" ]]; then
        continue
      fi

      # Extract DGP label
      DGP=${dgp_map["${ETA_TYPE}_${HTE_TYPE}_${tx_difficulty}"]}

      # Loop through sample sizes
      for N in $(jq -r '.n_list[]' "$PARAMS_FILE"); do
        LOG_FILE="/n/holylabs/LABS/pillai_lab/Users/xmeng1/CausalSurvival/scripts/datagen/R-code-logs/log_n${N}_eta${ETA_TYPE}_HTE${HTE_TYPE}_tx_difficulty${tx_difficulty}_iter${i}.log"

        [ "$VERBOSE" -eq 2 ] && echo "Running: n=$N, eta_type=$ETA_TYPE, HTE_type=$HTE_TYPE, tx_difficulty=$tx_difficulty, iteration=$i"

        singularity_command="singularity exec --cleanenv --env R_LIBS_USER=${my_packages} ${rstudio_singularity_image}"
        $singularity_command Rscript --vanilla ./scripts/datagen/simulate_data.R \
          --i "$i" \
          --n "$N" \
          --eta_type "$ETA_TYPE" \
          --HTE_type "$HTE_TYPE" \
          --tx_difficulty "$tx_difficulty" \
          --params_file "$PARAMS_FILE" \
          --verbose "$VERBOSE" > "$LOG_FILE" 2>&1

        if [ $? -eq 0 ]; then
          [ "$VERBOSE" -eq 2 ] && echo "Completed: n=$N, eta_type=$ETA_TYPE, HTE_type=$HTE_TYPE, tx_difficulty=$tx_difficulty, iteration=$i. Log: $LOG_FILE"
        else
          echo "Error: Simulation failed for n=$N, eta_type=$ETA_TYPE, HTE_type=$HTE_TYPE, tx_difficulty=$tx_difficulty, iteration=$i. Check log: $LOG_FILE"
        fi
      done
    done
  done
done