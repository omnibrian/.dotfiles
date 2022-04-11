# Export temporary credentials from assuming a role with cli based on
# configured awscli profile.

assume_version='0.1.2'

assume_usage() {
  cat <<EOF
Usage: aws-assume [-h | --help] [-v | --version] [--profile <profilename>] [--duration <seconds>]

Export temporary AWS credentials from STS assume-role based on configured
awscli profile. To stop using the assumed role, call the 'unassume' helper
function.

Available options:
  -h, --help      Print this help message and exit
  -v, --version   Print version and exit
  -p, --profile   AWS profile to load (default: default)
  -d, --duration  Duration in seconds for the temporary credentials (default: 8 hours)
EOF
}

if ! (return 0 2>/dev/null) ; then
  assume_usage
  echo
  echo -e "\033[0;31mThis script is meant to be sourced, please run again but with 'source' in front\033[0m"
  exit 1
fi

trap cleanup SIGINT SIGTERM ERR EXIT
cleanup() {
  trap - SIGINT SIGTERM ERR EXIT

  [[ -n "${assume_version-}" ]]           && unset assume_version
  [[ -n "${assume_profile-}" ]]           && unset assume_profile
  [[ -n "${assume_duration-}" ]]          && unset assume_duration
  [[ -n "${assume_access_key_id-}" ]]     && unset assume_access_key_id
  [[ -n "${assume_secret_access_key-}" ]] && unset assume_secret_access_key
  [[ -n "${assume_role_arn-}" ]]          && unset assume_role_arn
  [[ -n "${assume_source_profile-}" ]]    && unset assume_source_profile
  [[ -n "${assume_mfa_serial-}" ]]        && unset assume_mfa_serial
  [[ -n "${assume_mfa_token-}" ]]         && unset assume_mfa_token
  [[ -n "${assume_temp_creds-}" ]]        && unset assume_temp_creds
}

assume_parse_params() {
  assume_profile='default'
  assume_duration='28800'

  while : ; do
    case "${1-}" in
      -h | --help)
        assume_usage
        return 1
        ;;
      -v | --version)
        echo "aws-assume version: ${assume_version}"
        return 1
        ;;
      -p | --profile)
        assume_profile="${2-}"
        shift
        ;;
      -d | --duration)
        assume_duration="${2-}"
        shift
        ;;
      -?*)
        assume_usage
        echo -e "\nUnknown option: $1"
        return 1
        ;;
      *) break ;;
    esac
    shift
  done

  return 0
}
assume_parse_params "$@" || return $?

# get temporary credentials
assume_prompt_mfa() {
  echo -n "Enter MFA code: "
  read assume_mfa_token
}

assume_get_config() {
  aws configure get --profile ${assume_profile} ${1} 2>/dev/null
}

if [[ -n "$(assume_get_config role_arn)" ]] ; then
  assume_role_arn=$(assume_get_config role_arn)
  assume_source_profile=$(assume_get_config source_profile)
  assume_mfa_serial=$(assume_get_config mfa_serial)

  if [[ -n "${assume_mfa_serial}" ]] ; then
    assume_prompt_mfa
  fi

  assume_temp_creds=$(\
    aws sts assume-role \
      --profile ${assume_source_profile} \
      --role-arn ${assume_role_arn} \
      --serial-number ${assume_mfa_serial} \
      --token-code ${assume_mfa_token} \
      --role-session-name "assumed-role_${assume_role_arn##*/}" \
      --duration-seconds ${assume_duration} \
      --output json)
elif [[ -n "$(assume_get_config aws_access_key_id)" ]] ; then
  assume_access_key_id=$(assume_get_config aws_access_key_id)
  assume_secret_access_key=$(assume_get_config aws_secret_access_key)

  assume_mfa_serial=$(\
    aws sts get-caller-identity \
      --profile ${assume_profile} \
      --query Arn \
      --output text | sed 's|:user/|:mfa/|')
  assume_prompt_mfa

  assume_temp_creds=$(\
    aws sts get-session-token \
      --profile ${assume_profile} \
      --serial-number ${assume_mfa_serial} \
      --token-code ${assume_mfa_token} \
      --duration-seconds ${assume_duration} \
      --output json)
else
  echo "awscli profile (${assume_profile}) has no role_arn or aws_access_key_id configured."
  return 1
fi

# export temporary credentials
assume_get_cred() {
  if ! type jq &>/dev/null ; then
    echo "${assume_temp_creds}" | python3 -c "import sys,json; print(json.load(sys.stdin)['Credentials']['${1}'])"
  else
    echo ${assume_temp_creds} | jq -r ".Credentials.${1}"
  fi
}

export AWS_ACCESS_KEY_ID=$(assume_get_cred AccessKeyId)
export AWS_SECRET_ACCESS_KEY=$(assume_get_cred SecretAccessKey)
export AWS_SESSION_TOKEN=$(assume_get_cred SessionToken)
export AWS_DEFAULT_REGION=$(assume_get_config region)

# update PS1
if [[ -z "${VIRTUAL_ENV_DISABLE_PROMPT:-}" ]] ; then
  # work around virtualenv PS1 handling
  if [[ -n "${_OLD_VIRTUAL_PS1:-}" ]] ; then
    _OLD_ASSUME_VIRTUAL_PS1="${_OLD_VIRTUAL_PS1}"
    _OLD_VIRTUAL_PS1="(AWS:${assume_profile}) ${_OLD_VIRTUAL_PS1}"
  fi

  _OLD_ASSUME_PS1="${PS1}"
  PS1="(AWS:${assume_profile}) ${PS1}"
  export PS1
fi

# set "unassume" function for user to unset prompt and temporary credentials
unassume() {
  # work around virtualenv PS1 handling
  if [[ -n "${_OLD_VIRTUAL_PS1}" ]] ; then
    if [[ -n "${_OLD_ASSUME_VIRTUAL_PS1}" ]] ; then
      _OLD_ASSUME_PS1="${_OLD_ASSUME_PS1}"
      _OLD_VIRTUAL_PS1="${_OLD_ASSUME_VIRTUAL_PS1}"
      unset _OLD_ASSUME_VIRTUAL_PS1
    else
      _TEMP_ORIGINAL_PS1="${_OLD_ASSUME_PS1}"
      _OLD_ASSUME_PS1="${PS1//`echo ${_OLD_VIRTUAL_PS1}`}${_OLD_ASSUME_PS1}"
      _OLD_VIRTUAL_PS1="${_TEMP_ORIGINAL_PS1}"
      unset _TEMP_ORIGINAL_PS1
    fi
  elif [[ -n "${_OLD_ASSUME_VIRTUAL_PS1}" ]] ; then
    _OLD_ASSUME_PS1="${_OLD_ASSUME_VIRTUAL_PS1}"
    unset _OLD_ASSUME_VIRTUAL_PS1
  fi

  if [[ -n "${_OLD_ASSUME_PS1}" ]] ; then
    PS1="${_OLD_ASSUME_PS1}"
    export PS1
    unset _OLD_ASSUME_PS1
  fi

  unset AWS_ACCESS_KEY_ID
  unset AWS_SECRET_ACCESS_KEY
  unset AWS_SESSION_TOKEN
  unset AWS_DEFAULT_REGION

  unset -f unassume
}
