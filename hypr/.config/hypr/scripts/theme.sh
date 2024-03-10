#!/usr/bin/env bash

# Generate a color scheme from a random wallpaper
wal -q -i ~/st/wallpapers/ 

# Load the generated color scheme
source "$HOME/.cache/wal/colors.sh"

# Set wallpaper to the one selected by wal
swww img $wallpaper --transition-type outer --transition-step 5 --transition-fps 60
