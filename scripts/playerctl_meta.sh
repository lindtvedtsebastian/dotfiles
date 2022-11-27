#!/bin/bash

# Operate on any players, but prioritze cider
PLAYER_PRIORITY="cider,%any"

# Set status icon
if [ $(playerctl --player=$PLAYER_PRIORITY status) == "Playing" ];
then
	STATUS_ICON="⏸ "
else
	STATUS_ICON="⏵︎ "
fi

if [[ -z $(playerctl -l) ]];
then
	echo "No players available!"
else
	echo "$(playerctl metadata --player=$PLAYER_PRIORITY --format "{{artist}} - {{title}}") $STATUS_ICON"
fi
