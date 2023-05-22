#!/bin/bash
# https://discussions.citrix.com/topic/345960-xenserver-automated-snapshots-script/
#updated version that imports the snapshot to a specified destination machine:
# Configurations
SourceVM="<Source VM UUID>"
DestinationVM="<Destination VM UUID>"
DestinationPath="/mnt/"
backup_ext=".xva"
# Initialize variables
ret_code=0
snapshot_uuid=""
snapshot_name=""
imported_vm=""
echo "Starting migration..."
echo "Source VM: $SourceVM"
echo "Destination VM: $DestinationVM"
echo "Destination Path: $DestinationPath"
if [[ "$DestinationPath" != */ ]]; then
    DestinationPath="$DestinationPath/"
fi

# Get VM label
vm_label_raw=$(xe vm-param-get param-name=name-label uuid=$SourceVM)
if [ $? -ne 0 ]; then
    echo "Failed to get VM label for UUID: $SourceVM"
    ret_code=1
else
    vm_label=$(echo "$vm_label_raw" | tr ' ' _ | tr -d '(' | tr -d ')')

    # Prepare snapshot name
    date=$(date +%Y-%m-%d_%H-%M-%S)
    snapshot_name=$vm_label"_migration_"$date

    # Snapshot VM
    snapshot_uuid=$(xe vm-snapshot uuid=$SourceVM new-name-label=$snapshot_name)
    if [ $? -ne 0 ]; then
        echo "Failed to create snapshot for VM UUID: $SourceVM"
        ret_code=1
    else
        # Remove is-a-template attribute from snapshot
        echo=$(xe template-param-set is-a-template=false uuid=$snapshot_uuid)
        if [ $? -ne 0 ]; then
            echo "Failed to remove template attribute from VM UUID: $SourceVM"
            ret_code=1
        else
            echo "Snapshot created successfully. Start migration of $snapshot_name ..."

            # Export snapshot to destination path
            echo=$(xe vm-export uuid=$snapshot_uuid filename="$DestinationPath$snapshot_name$backup_ext")
            if [ $? -ne 0 ]; then
                echo "Failed to export snapshot: $snapshot_name$backup_ext"
                ret_code=1
            else
                echo "Migration of $snapshot_name completed successfully. Snapshot exported to: $DestinationPath"

                # Import snapshot as a new VM on the destination machine
                imported_vm=$(xe vm-import vm=$DestinationVM filename="$DestinationPath$snapshot_name$backup_ext")
                if [ $? -ne 0 ]; then
                    echo "Failed to import snapshot as a new VM: $snapshot_name$backup_ext"
                    ret_code=1
                else
                    echo "Snapshot imported successfully as a new VM: $imported_vm"
                fi
            fi

            # Remove temporary snapshot
            echo=$(xe vm-uninstall force=true uuid=$snapshot_uuid)
            if [ $? -ne 0 ]; then
                echo "Failed to remove temporary snapshot: $snapshot_name"
                ret_code=1
            fi
        fi
    fi
fi

exit $ret_code

# In this script, you need to specify the UUID of the destination machine using the DestinationVM variable. The script exports the snapshot from the source machine and imports it as a new VM on the specified destination machine.


# Now let me explain what each line does:

#!/bin/bash: This is a shebang that specifies the interpreter to be used to execute the script (in this case, bash).
# set -e: This tells the shell to exit immediately if any command exits with a non-zero status.
# SRC_UUID="4bce3dc4-a1f1-66c7-48b5-31536be6f123": This sets the UUID of the source VM.
# DST_UUID="278fc9f6-f377-fa30-bd54-a3b239027456": This sets the UUID of the destination VM.
# BACKUP_DIR="/mnt/test/": This sets the directory where the backup file will be saved.
# SNAPSHOT_NAME="snapshot_$(date +%Y-%m-%d_%H-%M-%S)": This generates a snapshot name based on the current timestamp, using the format snapshot_yyyy-mm-dd_HH-MM-SS.
# SNAPSHOT_UUID=$(xe vm-snapshot uuid="$SRC_UUID" new-name-label="$SNAPSHOT_NAME"): This creates a snapshot of the source VM with the given UUID and assigns the UUID of the new snapshot to the SNAPSHOT_UUID variable.
# xe snapshot-export-to-template uuid="$SNAPSHOT_UUID" filename="${BACKUP_DIR}/${SNAPSHOT_NAME}.xva": This exports the snapshot to a file in the backup directory, with the file name snapshot_yyyy-mm-dd_HH-MM-SS.xva.
# IMPORT_UUID=$(xe vm-import filename="${BACKUP_DIR}/${SNAPSHOT_NAME}.xva"): This imports the snapshot file to the destination VM and assigns the UUID of the new VM to the IMPORT_UUID variable.
# xe snapshot-destroy snapshot-uuid="$SNAPSHOT_UUID": This destroys the snapshot on the source VM with the given UUID.
# That's it! This script creates a snapshot of a source VM, exports it to a backup file, imports the backup file to a destination VM, and destroys the snapshot on the source VM.
