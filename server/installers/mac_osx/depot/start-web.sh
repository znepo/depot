#!/bin/bash
export DEPOT_HOME=`dirname $0` 
$DEPOT_HOME/bin/depot-http --logto=$DEPOT_HOME/logs/depot-http.log --quiet --wd=web
