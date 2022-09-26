#!/bin/bash

RED='\033[0;31m'
PURPLE='\033[0;35m'
GREEN='\033[0;32m'
RESET='\033[0m'

files=$(curl -X GET $1 2>&1 | grep latest.osm.pbf | awk '{print $6}' | awk -F "\"" '{print $2}')

for file in $files
do
	area=$(basename $file -latest.osm.pbf)
	cur_dir=$(pwd)
	if test -f "$file";
	then
		if test -f "$area.mbtiles";
		then
			echo -e "${RED}$area already exists as .mbtiles, skipping..${RESET}"
		else
			echo -e "${PURPLE} converting $file to .mbtiles${RESET}"
			(cd ~/git/tilemaker/ && tilemaker --input "$cur_dir/$file" --output "$pwd/$area.mbtiles")      
		fi
	else
		echo -e "${PURPLE}Downloading map data for ${GREEN}$area${RESET}"
		curl -O -X GET "$1$file"
		(cd ~/git/tilemaker/ && tilemaker --input "$cur_dir/$file" --output "$pwd/$area.mbtiles")      
	fi
done
