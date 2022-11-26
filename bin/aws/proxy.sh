#!/usr/bin/env bash
#
# Start localhost binding through SSM session to proxy server.

set -euo pipefail

usage() {
  cat <<EOF
Usage: aws-proxy [-h | --help] [--profile <profile>] [--region <region>] [--name <instancename>]
           [--local-port <localport>] [--remote-port <remoteport>] [--port <port>]

Start forward local port to proxy server through SSM session.

Available options:
  -h, --help        Print this help message and exit
  -p, --profile     AWS profile to load
  -r, --region      AWS region to use
  -n, --name        Name of instance to use
  --port            Set both local and remote ports
  -l, --local-port  Local port to use
  --remote-port     Remote port to use
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
  instance_name="QRoC-Jump-Server"
  local_port="3128"
  remote_port="3128"

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
      --port)
        local_port="${2-}"
	remote_port="${2-}"
	shift
	;;
      -l | --local-port)
        local_port="${2-}"
        shift
        ;;
      --remote-port)
        remote_port="${2-}"
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

instance_id=$(\
  aws ${aws_params} ec2 describe-instances \
  --filter "Name=tag:Name,Values=${instance_name}" Name=instance-state-name,Values=running \
  --query 'Reservations[].Instances[0].InstanceId')

aws ${aws_params} ssm start-session \
  --document-name AWS-StartPortForwardingSession \
  --parameters "portNumber=[${remote_port}],localPortNumber=[${local_port}]" \
  --target ${instance_id}
