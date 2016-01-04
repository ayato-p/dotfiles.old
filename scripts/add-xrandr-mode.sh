#!/bin/bash

xrandr --newmode "1920x1080" 173.00 1920 2048 2248 2576 1080 1083 1088 1120 -hsync +vsync
xrandr --addmode Virtual1 1920x1080

xrandr --newmode "1600x900" 118.25 1600 1696 1856 2112 900 903 908 934 -hsync +vsync
xrandr --addmode Virtual1 1600x900
