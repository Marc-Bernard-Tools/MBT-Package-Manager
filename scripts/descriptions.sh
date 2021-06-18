#!/bin/bash

#
# Add MBT prefix to descriptions in XML files
#

xmlfiles="$2/*.clas.xml $2/*.intf.xml"

for xmlfile in $xmlfiles
do
  echo "Adding MBT prefix to description in $xmlfile"
  sed -i "0,/^(.*\<DESCRIPT\>)(.*)$/ s/^(.*\<DESCRIPT\>)(.*)$/\1MBT \2/" $xmlfile
done
