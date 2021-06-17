#!/bin/bash

#
# Insert credits into CLAS and INTF files (right after first statement)
#

# Get credits
credits=$(<$1)

# Escape it for use as a Sed replacement string
IFS= read -d '' -r < <(sed -e ':a' -e '$!{N;ba' -e '}' -e 's/[&/\]/\\&/g; s/\n/\\&/g' <<<"$credits")
replace=${REPLY%$'\n'}

abapfiles="$2/*.clas.abap $2/*.intf.abap"

for abapfile in $abapfiles
do
  echo "Adding credits to $abapfile"
  sed -i "0,/^$/ s/^$/\n$replace\n/" $abapfile
done
