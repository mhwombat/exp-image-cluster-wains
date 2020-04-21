#!/bin/bash
source ./config.sh
./trendSummary.sh | csvcut -c 1,17,38,21,40,24,42 | csvlook

