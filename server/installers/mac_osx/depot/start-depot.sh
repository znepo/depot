#!/bin/bash
export DEPOT_HOME=`dirname $0` 
$DEPOT_HOME/bin/depot --wd=data --logto=$DEPOT_HOME/logs/depot.log

