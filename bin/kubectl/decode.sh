#!/usr/bin/env bash
#
# Decode the contents of base64 encoded kubectl secret output.

set -euo pipefail

usage() {
	cat >&2 <<EOF
Usage: kubectl decode secret SECRET_NAME [kubectl args] [-o|--output yaml|json]

Decode kubernetes secrets without having to pull out a base64 decoder.

Examples:
  kubectl decode secret my-secret
  kubectl decode secret -n my-namespace my-secret
  kubectl decode secret my-secret --context prod -o json
  kubectl decode secret my-secret --namespace kube-system -o yaml

Options:
  -h, --help	Print this help message and exit
  -o, --output	Output formatting to display (default: 'yaml' if 'yq' is installed, otherwise 'json')
EOF
}

msg() {
	echo >&2 -e "${1-}"
}

debug() {
	if [[ "${DEBUG-false}" == "true" ]]; then
	       msg "DEBUG: ${1-}"
	fi
}

die() {
	msg "${1}"
	exit "${2-1}"  # default exit status of 1
}

parse_params() {
	resource="$1"
	shift
	if [[ "$resource" != "secret" ]]; then
		usage
		die "\nERROR: only 'secret' resources are supported. Got '$resource'."
	fi
	debug "resource is '$resource'"

	resource_name="$1"
	shift
	if [[ -z "$resource_name" ]]; then
		usage
		die "\nERROR: resource name is required."
	fi
	debug "resource_name is '$resource_name'"

	kubectl_opts=""
	output="yaml"

	while : ; do
		case "${1-}" in
			-h | --help)
				usage
				exit
				;;
			-o | --output)
				if [[ $# -lt 2 ]]; then
					die "ERROR: missing value for $1"
				fi
				debug "found output opt '$1' setting to '$2'"
				output="${2}"
				shift 2
				;;
			-o=* | --output=*)
				debug "got output opt '$1'"
				output="${1#*=}"
				shift
				;;
			*)
				debug "unhandled opt: '${1-}'"
				if [[ -z "${1-}" ]]; then
					debug "opt is empty, ending arg parsing loop"
					break
				else
					debug "assuming opt '${1-}' is a kubectl opt"
					kubectl_opts+=" ${1-}"
					shift
				fi
				;;
		esac
	done
}

parse_params "$@"

if ! command -v jq >/dev/null 2>&1; then
	die "ERROR: 'jq' is required but could not be found on PATH."
fi

if [[ "$output" == "yaml" ]] && ! command -v yq >/dev/null 2>&1; then
	die "ERROR: 'yq' is required for 'yaml' output format. 'yq' not found on PATH."
fi

debug "executing: kubectl get '$resource' '$resource_name' -o json ${kubectl_opts}"
json_output="$(kubectl get "$resource" "$resource_name" -o json ${kubectl_opts} | jq '.data |= with_entries(.value |= @base64d)')"

if [[ "$output" == "json" ]]; then
	echo "$json_output"
elif [[ "$output" == "yaml" ]]; then
	echo "$json_output" | yq -P
else
	die "ERROR: output type '$output' not recognize"
fi
