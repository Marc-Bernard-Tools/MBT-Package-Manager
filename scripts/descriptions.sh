#!/bin/bash

#
# Add MBT prefix to descriptions in XML files
#

xmlfiles="$1/*.clas.xml $1/*.intf.xml"

for xmlfile in $xmlfiles
do
  if [[ -e $xmlfile ]]; then   
    echo "Adding prefix to description in $xmlfile"
    sed -i '0,/<DESCRIPT>/ s|^\(.*<DESCRIPT>\)\(.*\)\(</DESCRIPT>\)|\1MBT \2\3|g' $xmlfile
    sed -i '0,/<OBJ_NAME>/ s|^\(.*<OBJ_NAME>\)ZCX_LOGGER\(</OBJ_NAME>\)|\1/MBTOOLS/CX_LOGGER\2|g' $xmlfile
    sed -i '0,/<OBJ_NAME>/ s|^\(.*<OBJ_NAME>\)ZCX_LOGGER_DISPLAY_PROFILE\(</OBJ_NAME>\)|\1/MBTOOLS/CX_LOGGER_DISP_PROF\2|g' $xmlfile
  fi
done
