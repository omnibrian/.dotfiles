#!/usr/bin/env bash
#
# Entrypoint for WM binding to start polybar

if [[ -z "$(pgrep -u $UID -x polybar)" ]] ; then
  # start polybar for all known monitor connections
  for monitor in $(xrandr -q | grep connected | cut -d' ' -f1) ; do
    MONITOR=$monitor polybar --quiet --reload main &
  done
else
  # send restart command to existing polybar instances
  polybar-msg cmd restart
fi
