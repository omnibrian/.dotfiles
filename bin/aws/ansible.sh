#!/usr/bin/env bash -euo pipefail
#
# Execute ansible playbook from S3 against a target instance.

usage() {
  cat <<EOF
Usage: aws-ansible [-h | --help] [--profile <profile>] [--region <region>] [--bucket <bucket>]
           [--cloudwatch <cloudwatchloggroup>] [--extra-variables "<key>=<value>"]
           PLAYBOOK_FILE INSTANCE_ID

Execute ansible playbook from S3 against a target instance.

Available options:
  -h, --help             Print this help message and exit
  -p, --profile          AWS profile to load
  -r, --region           AWS region to use
  -b, --bucket           Name of the S3 bucket to download ansible playbook from (default: ansible-development)
  -l, --cloudwatch       Name of CloudWatch log group to output to
  -e, --extra-variables  Extra variables to pass to ansible-playbook (space-separated k=v list)
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
  bucket="ansible-development"
  cloudwatch="ansible_development"
  extra_variables=""

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
      -b | --bucket)
        bucket="${2-}"
        shift
        ;;
      -l | --cloudwatch)
        cloudwatch="${2-}"
        shift
        ;;
      -e | --extra-variables)
        extra_variables=" ${2-}"
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

  args=("$@")

  [[ ${#args[@]} -eq 0 ]] && die "Missing required arguments for PLAYBOOK_FILE and INSTANCE_ID"
  playbook_file="${args[0]}"
  [[ ${#args[@]} -eq 1 ]] && die "Missing required 2nd argument for INSTANCE_ID"
  instance_id="${args[1]}"

  return 0
}
parse_params "$@"

send_command=$(aws ${aws_params} ssm send-command \
  --output json \
  --document-name "AWS-ApplyAnsiblePlaybooks" \
  --document-version "1" \
  --instance-ids "${instance_id}" \
  --parameters "SourceType=[\"S3\"],SourceInfo=[\"{\\\"path\\\": \\\"https://${bucket}.s3.amazonaws.com\\\"}\"],InstallDependencies=[\"True\"],PlaybookFile=[\"${playbook_file}\"],ExtraVariables=[\"SSM=True${extra_variables}\"],Check=[\"False\"],Verbose=[\"-v\"],TimeoutSeconds=[\"3600\"]" \
  --timeout-seconds 600 \
  --max-concurrency '50' \
  --max-errors '0' \
  --cloud-watch-output-config "CloudWatchOutputEnabled=true,CloudWatchLogGroupName=\"${cloudwatch}\"")

echo "Playbook started, use Control-C to exit from tailing output"

aws ${aws_params} logs tail \
  $(echo ${send_command} | jq -r '.Command.CloudWatchOutputConfig.CloudWatchLogGroupName') \
  --since 0s \
  --format short \
  --follow
