#!/bin/sh

pwd=`pwd`
basedir=`basename "$pwd"`

date=`echo "$basedir"|sed 's/^[^-]\+-[^-]\+-\([0-9]\+\)$/\1/p; d'`

if test "$date" == ""; then
    echo "Invalid package name $basedir."
    exit 1
else
    echo "Setting release to $date in Version.hs"
    perl -p -i -e "s/yyyymmdd/$date/" Version.hs
fi
