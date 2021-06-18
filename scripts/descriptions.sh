#!/bin/bash

#
# Add MBT prefix to descriptions in XML files
#

xmlfiles="$1/*.clas.xml $1/*.intf.xml"

for xmlfile in $xmlfiles
do
  echo "Adding prefix to description in $xmlfile"
  sed -i '0,/<DESCRIPT>/ s|^\(.*<DESCRIPT>\)\(.*\)\(</DESCRIPT>\)|\1MBT \2\3|g' $xmlfile
done
