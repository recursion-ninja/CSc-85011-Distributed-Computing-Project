#!/bin/bash
 
 
# [usage & Config]
# put this script on Xenserver master and execute with root privilege
# Change VM UUID(s) and the location to export
# Configs:
# VMs: a list of VM uuid, separated by comma
# ExportPath: the mount path to store exported xva file
# 
# [How it work]
# step1: iterate specified VM UUID(s) and create a snapshot for each
# step2: backup the snapshots to specified location
# step3: delete temporary snapshot created in step 1
# 
# [Note]
# please make sure you have enough disk space for ExportPath, or backup will fail
# tested on Xenserver 5.6 sp2 and 6.2
# Xenserver 6.2's performance is at least 4 times better than Xenserver 5.6
# on error, script will print the reason, and proceed to handle next VM 
# backed up file format: vm_label + "backup" + date of snapshot, i.e, win71_backup_2013-12-31_17-11-47.xva
#
 
##### Config #######
 
VMs="f5f5ba50-a2de-4b33-97a3-25236a6a039c"
ExportPath="/users/rwong1"
 
####################
 
 
vm_array=(${VMs//,/ })
ret_code=0
snapshot_uuid_array=
snapshot_name_array=
backup_ext=".xva"
 
echo "Starting to backup..."
echo "VM list: ${vm_array[@]}"
echo "ExportPath: ${ExportPath}"
 
if [[ "$ExportPath" != */ ]]; then
    ExportPath="$ExportPath/"
fi
 
for i in "${!vm_array[@]}"; do
    # get vm label
    echo "in loop ${vm_array[$i]}"
    uuid=${vm_array[$i]}
    vm_label_raw=`xe vm-param-get param-name=name-label uuid=$uuid`
    echo "this is $uuid"
    if [ $? -ne 0 ]; then
        echo "failed to get VM label uuid = $uuid"
        ret_code=1
	#echo $ret_code
        continue
    fi
    #vm_label=`echo "$vm_label_raw" | tr ' ' _ | tr -d '(' | tr -d ')'`
 
    # prepare snapshot name
    #date=$(date +%Y-%m-%d_%H-%M-%S)
    #snapshot_name=$vm_label"_backup_"$date
    #echo "$date"	
    # snapshot vm
    #snapshot_uuid=`xe vm-snapshot uuid=$uuid new-name-label=$snapshot_name`
   # if [ $? -ne 0 ]; then
    #    echo "failed to snapshot VM uuid = $uuid"
     #   ret_code=1
      #  continue
   # fi
    #snapshot_uuid_array[$i]=$snapshot_uuid
    #snapshot_name_array[$i]=$snapshot_name
    
    # remove is-a-template attribute from snapshot
    #echo=`xe template-param-set is-a-template=false uuid=$snapshot_uuid`
    #if [ $? -ne 0 ]; then
     #   echo "failed to remove template attribute from VM uuid = $uuid"
      #  ret_code=1
   # fi
done
 
 
# backup each VM to specified path and delete
#for i in "${!snapshot_uuid_array[@]}"; do
 #   snapshot_uuid=${snapshot_uuid_array[$i]}
  #  snapshot_name=${snapshot_name_array[$i]}
 
   # echo "Start backup $snapshot_name ..."
   # echo=`xe vm-export uuid=$snapshot_uuid filename="$ExportPath$snapshot_name$backup_ext"`
   # if [ $? -ne 0 ]; then
    #    echo "failed to export snapshot name = $snapshot_name$backup_ext"
     #   ret_code=1
   # else    
    #    echo "Successfully backup $snapshot_name to $ExportPath"
   # fi
 
    #echo=`xe vm-uninstall force=true uuid=$snapshot_uuid`
   # if [ $? -ne 0 ]; then
    #    echo "failed to remove temporary snapshot name = $snapshot_name"
     #   ret_code=1 
   # fi
#done
 
exit $ret_code    
