#!/usr/bin/env bash
#
# List all CloudTrail errors from the last hour.

set -euo pipefail

usage() {
  cat <<EOF
Usage: aws-errors [-h | --help] [--profile <profile>] [--region <region>] [--hours <hours>]

List all CloudTrail errors from the last hour.

Available options:
  -h, --help        Print this help message and exit
  -p, --profile     AWS profile to use
  -r, --region      AWS region to use
  --hours           Number of hours in the past to list errors for
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
  hours=1

  while : ; do
    case "${1-}" in
      -h | --help)
        usage
        exit
        ;;
      -p | --profile)
        aws_params="${aws_params} --profile ${2-}"
        shift
        ;;
      -r | --region)
        aws_params="${aws_params} --region ${2-}"
        shift
        ;;
      --hours)
        hours="${2-}"
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

(
  echo 'Time,Identity ARN,Event ID,Service,Action,Error,Message';
  aws ${aws_params} cloudtrail lookup-events \
      --start-time "$(date --utc -d "${hours} hours ago" +'%s')" \
      --end-time "$(date --utc +'%s')" \
      --query 'Events[*].CloudTrailEvent' \
      --output text \
    | jq -r '. | select(.eventType == "AwsApiCall" and .errorCode != null) | [.eventTime, .userIdentity.arn, .eventID, .eventSource, .eventName, .errorCode, .errorMessage] | @csv'
) | column -t -s'",'
