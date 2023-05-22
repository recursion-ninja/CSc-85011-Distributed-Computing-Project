#!/bin/bash

# Configuration
SourceVM="f5f5ba50-a2de-4b33-97a3-25236a6a039c"
SnapshotName="migration_snapshot"
LogFilePath="/users/rwong1/logfile.log"

# Function to log messages
log_message() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') $1" >> "$LogFilePath"
}

# Create the snapshot
log_message "Creating snapshot..."
snapshot_uuid=$(xe vm-snapshot vm=$SourceVM new-name-label=$SnapshotName)
if [ $? -ne 0 ]; then
    log_message "Failed to create snapshot."
    exit 1
fi

# Export the snapshot to a file
log_message "Exporting snapshot..."
export_filename="$SnapshotName.xva"
xe snapshot-export-to-template snapshot-uuid=$snapshot_uuid filename=$export_filename
if [ $? -ne 0 ]; then
    log_message "Failed to export snapshot."
    exit 1
fi

# Generate a new UUID for the destination VM
log_message "Generating new UUID for destination VM..."
DestinationVM=$(xe vm-clone vm=$SourceVM)
if [ $? -ne 0 ]; then
    log_message "Failed to generate new UUID for destination VM."
    exit 1
fi

# Import the snapshot to the destination VM
log_message "Importing snapshot to destination VM..."
imported_vm=$(xe vm-import filename=$export_filename sr-uuid=$(xe pool-list --minimal) preserve=true)
if [ $? -ne 0 ]; then
    log_message "Failed to import snapshot to destination VM."
    exit 1
fi
xe vm-param-set uuid=$imported_vm name-label=$DestinationVM

# Clean up the exported file
log_message "Cleaning up..."
rm $export_filename

log_message "Migration completed successfully."

