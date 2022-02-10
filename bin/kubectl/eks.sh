#!/usr/bin/env bash

if [[ -z "$1" ]]
then
    echo "Please supply one of [clusters|update-config]"
    exit 0
fi

command=$1
shift

case $command in
clusters)
    AWS_PAGER='' aws eks list-clusters --query clusters --output text $@
    ;;
update-config)
    aws eks update-kubeconfig $@
    ;;
*)
    aws eks $@
esac

