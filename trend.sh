#!/bin/bash
source ./config.sh
echo -e "Round\t$1"
grep "Summary.*$1" $log | sed "s/^[^\t]*\t//; s/[^\t]*$1=//; s/,.*//"

