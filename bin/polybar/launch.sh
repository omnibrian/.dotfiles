#!/usr/bin/env bash
#
# Entrypoint for WM binding to start polybar

# cleanup currently running polybars
killall -q polybar

# wait until processes are shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# launch new polybar
polybar --quiet --reload main
