#!/bin/sh

source ./config.sh
grep " agrees" $log | \
  tail -n 5000 | \
  sed 's/.* that //; s/has label //; s/\(.*\) \(.*\)/\2 \1/' | \
  sort | \
  grep Image | \
  sed 's/_[0-9]*.png//' | \
  uniq -c | \
  grep '^ *[0-9][0-9][0-9] '

