#!/bin/bash

if [[ -z $(playerctl -l) ]];
then
	echo "No players available!"
else
	playerctl previous
fi
