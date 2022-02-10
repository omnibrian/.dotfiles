#!/usr/bin/env bash -euo pipefail
#
# List EC2 instance names and instance IDs.

usage() {
  cat <<EOF
Usage: aws-instances [-h | --help] [--profile <profile>] [--region <region>] [--output <output>]

List EC2 instance names and instance IDs.

Available options:
  -h, --help        Print this help message and exit
  -p, --profile     AWS profile to load
  -r, --region      AWS region to use
  -o, --output      Output type to use from aws cli (default: table)
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
  aws_params=""
  local output="table"

  while : ; do
    case "${1-}" in
      -h | --help)
        usage
        exit
        ;;
      -p | --profile)
        aws_params+=" --profile ${2-}"
        shift
        ;;
      -r | --region)
        aws_params+=" --region ${2-}"
        shift
        ;;
      -o | --output)
        output="${2-}"
        shift
        ;;
      -?*)
        usage
        die "\nUnknown option: $1"
        ;;
      *) break ;;
    esac
    shift
  done

  aws_params+=" --output ${output}"

  return 0
}
parse_params "$@"

aws ${aws_params} ec2 describe-instances \
  --query 'Reservations[].Instances[].{Name:Tags[?Key==`Name`]|[0].Value,InstanceId:InstanceId,IP:PrivateIpAddress,State:State.Name}'
