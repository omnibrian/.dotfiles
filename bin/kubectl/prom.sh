#!/usr/bin/env bash
#
# Forward port for prometheus on cluster to localhost

( sleep 5; open http://localhost:9090/targets; ) &
kubectl port-forward svc/prometheus-k8s -n monitoring 9090:9090
