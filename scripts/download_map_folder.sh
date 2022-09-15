#!/bin/bash

RED='\033[0;31m'
PURPLE='\033[0;35m'
GREEN='\033[0;32m'
RESET='\033[0m'

files=$(curl -X GET $1 2>&1 | grep latest.osm.pbf | awk '{print $6}' | awk -F "\"" '{print $2}')

for file in $files
do
	area=$(basename $file -latest.osm.pbf)
	if test -f "$file";
	then
		echo -e "${RED}$area already exists, skipping..${RESET}"
	else
		echo -e "${PURPLE}Downloading map data for ${GREEN}$area${RESET}"
		curl -O -X GET "$1$file"
	fi
done
