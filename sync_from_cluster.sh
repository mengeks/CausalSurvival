#!/bin/bash

# Define the remote cluster path and local path
REMOTE_PATH="xmeng1@login.rc.fas.harvard.edu:/n/holylabs/LABS/pillai_lab/Users/xmeng1/CausalSurvival"
LOCAL_PATH="/Users/xmeng/Dropbox (Harvard University)/Xiang_Iav/CausalSurvival"

# Function to sync a folder from the cluster to the local machine
sync_folder() {
    local remote_dir=$1
    local local_dir=$2
    echo "Syncing $remote_dir to $local_dir"
    rsync -av --progress "$REMOTE_PATH/$remote_dir" "$local_dir"
}

# Sync the specified directories
sync_folder "scripts/TV-CSL/tables/" "$LOCAL_PATH/scripts/TV-CSL/tables/"
sync_folder "tables/" "$LOCAL_PATH/tables/"
sync_folder "scripts/TV-CSL/results/" "$LOCAL_PATH/scripts/TV-CSL/results/"
sync_folder "results/" "$LOCAL_PATH/results/"
sync_folder "figures/" "$LOCAL_PATH/figures/"

# Print a message when the sync is complete
echo "Sync complete."


