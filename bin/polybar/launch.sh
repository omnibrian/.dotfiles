#!/usr/bin/env bash
#
# Entrypoint for WM binding to start polybar

# cleanup running polybar instances
killall -q polybar

# start polybar for all known monitor connections
for monitor in $(xrandr -q | grep connected | cut -d' ' -f1) ; do
  MONITOR=$monitor polybar --quiet --reload main &
done
