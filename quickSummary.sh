#!/bin/bash
source ./config.sh
./trendSummary.sh | csvcut -c 1,17,21,24,62,63 | csvlook

