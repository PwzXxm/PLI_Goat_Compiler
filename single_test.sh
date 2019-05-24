#!/bin/sh

./Goat $1 > ./build/tmp.oz 

rc=$?
if [ $rc -ne 0 ]; then
    cat ./build/tmp.oz
    exit $rc
fi

./oz ./build/tmp.oz