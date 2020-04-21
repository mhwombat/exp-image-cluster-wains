#!/bin/sh
grep " agrees .* Image" $log | sed 's/.*\t//; s/ that.*//; s/ agrees with /\n/' | sort | uniq -c | sort -n
