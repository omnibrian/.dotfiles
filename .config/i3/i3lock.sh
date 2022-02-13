#!/usr/bin/env bash
#
# just a simple wrapper for i3lock to make it easier to call

i3lock \
  --image ~/Pictures/backgrounds/rhel_hello_world.jpg \
  --fill \
  --ringver-color '#61afefff' \
  --insidever-color '#61afef66' \
  --ringwrong-color '#ee0000ff' \
  --insidewrong-color '#ee000066' \
  --ring-color '#ee000088' \
  --keyhl-color '#ee0000bb' \
  --separator-color '#101114ff' \
  --inside-color '#00000000' \
  --clock \
  --time-str '%H:%M' \
  --time-color '#c8ccd5ff' \
  --date-color '#c8ccd5ff'
