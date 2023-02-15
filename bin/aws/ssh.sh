#!/usr/bin/env bash
#
# Start SSM session using instance name.

set -euo pipefail

usage() {
  cat <<EOF
Usage: aws-proxy --name <instancename> [-h | --help] [--profile <profile>] [--region <region>]

Start SSM session to remote server.

Available options:
  -h, --help        Print this help message and exit
  -p, --profile     AWS profile to load
  -r, --region      AWS region to use
  -n, --name        Name of instance to use
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
  aws_params="--output text"
  instance_name=""

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
      -n | --name)
        instance_name="${2-}"
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

  return 0
}
parse_params "$@"

if [[ -z "${instance_name}" ]]; then
  die "Required --name flag not found"
fi

instance_id=$(\
  aws ${aws_params} ec2 describe-instances \
  --filter "Name=tag:Name,Values=${instance_name}" Name=instance-state-name,Values=running \
  --query 'Reservations[].Instances[0].InstanceId')

if [[ -z "${instance_id}" ]]; then
  die "No instances found with name '${instance_name}'"
fi

aws ${aws_params} ssm start-session \
  --target "${instance_id}"
