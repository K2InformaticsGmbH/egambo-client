#!/bin/bash

unamestr=`uname`
if [[ "$unamestr" == 'Linux' || "$unamestr" == 'Darwin' ]]; then
     exename=erl
else
    exename='start //MAX werl.exe'
    #exename='erl.exe'
fi

# PATHS
paths="-pa"
paths=$paths" _build/default/lib/*/ebin"
paths=$paths" _checkouts/*/ebin"

echo "------------------------------------------"
echo "Starting egambo_client"
echo "------------------------------------------"

# Starting dderl
$exename