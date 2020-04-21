#!/bin/bash
source ./config.sh
grep "Image .* has adjusted novelty" $log | sed 's/.*, Image \(.*\) has adjusted novelty \(.*\)/\1 \2/' | sort -k1,1 -k2,2nr | sort -k1,1 -u | sort -k2,2n

