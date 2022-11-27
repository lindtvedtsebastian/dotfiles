#!/bin/bash

# Operate on any players, but prioritze cider
PLAYER_PRIORITY="cider,%any"

if [[ -z $(playerctl -l) ]];
then
	echo "No players available!"
else
	echo "$(playerctl metadata --player=$PLAYER_PRIORITY --format "{{artist}} - {{title}}")"
fi
