#!/bin/bash

# Operate on any players, but prioritze cider
PLAYER_PRIORITY="cider,%any"

if [[ -z $(playerctl -l) ]];
then
	echo "No players available!"
else
	if [ $(playerctl --player=$PLAYER_PRIORITY status) == "Playing" ];
	then 
		playerctl --player=$PLAYER_PRIORITY pause
	else
		playerctl --player=$PLAYER_PRIORITY play
	fi
fi
