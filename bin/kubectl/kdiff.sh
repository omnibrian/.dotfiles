#!/usr/bin/env bash
#
# Run kustomize on any cluster folder that different from master
# and output diff

cd `git rev-parse --show-toplevel`

if [[ -n "$1" ]]; then
  clusters=`echo $1 | cut -d/ -f2`
else
  clusters=`git diff HEAD..master --name-only | grep '^clusters/' | cut -d/ -f2 | sort | uniq`
fi

for cluster in `echo $clusters`
do
  kustomize build --load-restrictor LoadRestrictionsNone clusters/$cluster > ../$cluster.after.yaml
done

git co master

for cluster in `echo $clusters`
do
  kustomize build --load-restrictor LoadRestrictionsNone clusters/$cluster > ../$cluster.before.yaml
done

git co -

for cluster in `echo $clusters`
do
  diff ../$cluster.before.yaml ../$cluster.after.yaml
done
