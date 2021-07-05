#!/bin/bash

#
# Fix various abaplint issues
#

abapfiles="$1/*.abap"

for abapfile in $abapfiles
do
  if [[ -e $abapfile ]]; then   
    echo "Fixing abaplint issues in $abapfile"
    sed -i 's|CONSTANTS version|CONSTANTS c_version|g' $abapfile
    sed -i 's|tty_entries|ty_entries|g' $abapfile
    sed -i 's|tts_entries|ty_entries_ts|g' $abapfile
    sed -i 's| me-\>| |g' $abapfile
    sed -i 's|lty_pair|ty_pair|g' $abapfile
  fi
done
