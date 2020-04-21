#!/bin/bash
source ./config.sh

grep 'Summary - avg.' $log | \
  sed 's/.*\t\([0-9]*\)\tSummary - /round=\1,/; 1d; 2s/\(.*\)/\1\n\1/' | \
  sed '1s/=[^,]*//g; 2,$s/round=//; 2,$s/,[^=]*=/,/g'

# | csvcut -c 36,38,40,42,44,46,48,49,50,52,53,55,61,17,21,24 | sed 's/,/\t/g'
# csvcut -c 1,17,21,24,61 | csvlook

