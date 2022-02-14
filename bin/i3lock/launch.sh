#!/usr/bin/env bash
#
# simple i3lock wrapper to make it easier to configure

set -eo pipefail

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME/.config"}
CONFIG="${XDG_CONFIG_HOME}/i3lock/config"

usage() {
  cat <<EOF
Usage: $0 [-h | --help] [-c <config> | --config <config>] [-n | --nofork]

Wrapper for i3lock-color in order to use a config file.

Available options:
  -c, --config  Location for i3lock config file (default location: ${CONFIG})
  -n, --nofork  Passes through --nofork to i3lock
  -h, --help    Print this help message and exit
EOF
}

msg() {
  echo >&2 -e "${1-}"
}

die() {
  msg "${1}"
  exit "${2-1}"  # default exit status of 1
}

parse_params() {

  NOFORK_OVERRIDE=false

  while : ; do
    case "${1-}" in
      -c | --config)
        CONFIG="${2-}"
	shift
	;;
      -n | --nofork)
        NOFORK_OVERRIDE=true
	;;
      -h | --help)
        usage
	exit
	;;
      -?*)
        usage
	die "Unknown option: $1"
	;;
      *) break ;;
    esac
    shift
  done
}

parse_params "$@"

if [[ -f "${CONFIG}" ]] ; then
  source "${CONFIG}"
fi

i3lock_args=""

add_args() {
  # usage:
  #   add_args flag <config var name> <i3lock flag name>
  #   add_args option <config var name> <i3lock option name>

  opt_name="${2}"
  var_name="$(echo ${2} | awk '{ gsub(/^\-\-/, "", $0); gsub(/\-/, "_", $0); print toupper($0) }')"

  case "${1}" in
    flag)
      if [[ -n "${!var_name}" ]] && ! [[ "${!var_name}" =~ false|no|off ]] ; then
        i3lock_args+=" ${opt_name}"
      fi
      ;;
    option)
      if [[ -n "${!var_name}" ]] ; then
        i3lock_args+=" ${opt_name}='${!var_name}'"
      fi
      ;;
    *)
      msg "'${1}' must be 'flag' or 'option', skipping..."
      ;;
  esac
}

# special handling for --nofork
if [[ "${NOFORK_OVERRIDE}" == "true" ]] || [[ "${NOFORK}" == "true" ]] ; then
  i3lock_args+=" --nofork"
fi

#> i3lock options
add_args flag   --beep
add_args flag   --no-unlock-indicator
add_args option --image
add_args option --raw
add_args option --color
add_args flag   --tiling
add_args flag   --centered
add_args flag   --fill
add_args flag   --max
add_args flag   --scale
add_args option --pointer
add_args flag   --ignore-empty-password
add_args flag   --show-failed-attempts
add_args flag   --debug

#> i3lock-color options
add_args option --screen
add_args option --blur
add_args flag   --clock
add_args flag   --force-clock
add_args flag   --indicator
add_args option --radius
add_args option --ring-width
add_args option --inside-color
add_args option --ring-color
add_args option --insidever-color
add_args option --ringver-color
add_args option --insidewrong-color
add_args option --ringwrong-color
add_args option --line-color
add_args flag   --line-uses-inside
add_args flag   --line-uses-ring
add_args option --keyhl-color
add_args option --bshl-color
add_args option --separator-color
add_args option --verif-color
add_args option --wrong-color
add_args option --modif-color
add_args option --layout-color
add_args option --time-color
add_args option --date-color
add_args option --greeter-color
add_args option --time-str
add_args option --date-str
add_args option --verif-text
add_args option --wrong-text
add_args option --keylayout
add_args option --noinput-text
add_args option --lock-text
add_args option --lockfailed-text
add_args option --greeter-text
add_args flag   --no-modkey-text
add_args option --time-align
add_args option --date-align
add_args option --layout-align
add_args option --verif-align
add_args option --wrong-align
add_args option --modif-align
add_args option --greeter-align
add_args option --timeoutline-color
add_args option --dateoutline-color
add_args option --layoutoutline-color
add_args option --verifoutline-color
add_args option --wrongoutline-color
add_args option --greeteroutline-color
add_args option --modifoutline-color
add_args option --time-font
add_args option --date-font
add_args option --layout-font
add_args option --verif-font
add_args option --wrong-font
add_args option --greeter-font
add_args option --time-size
add_args option --date-size
add_args option --layout-size
add_args option --verif-size
add_args option --wrong-size
add_args option --greeter-size
add_args option --timeoutline-width
add_args option --dateoutline-width
add_args option --layoutoutline-width
add_args option --verifoutline-width
add_args option --wrongoutline-width
add_args option --greeteroutline-width
add_args option --modifieroutline-width
add_args option --ind-pos
add_args option --time-pos
add_args option --date-pos
add_args option --greeter-pos
add_args flag   --pass-media-keys
add_args flag   --pass-screen-keys
add_args flag   --pass-power-keys
add_args flag   --pass-volume-keys
add_args flag   --bar-indicator
add_args option --bar-direction
add_args option --bar-orientation
add_args option --bar-step
add_args option --bar-max-height
add_args option --bar-base-width
add_args option --bar-color
add_args option --bar-periodic-step
add_args option --bar-pos
add_args option --bar-count
add_args option --bar-total-width
add_args flag   --redraw-thread
add_args option --refresh-rate
add_args flag   --composite
add_args flag   --no-verify
add_args option --slideshow-interval
add_args flag   --slideshow-random-selection

# i know it's hacky but it's working for me ¯\_(ツ)_/¯
eval /usr/bin/i3lock ${i3lock_args}
