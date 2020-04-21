#!/bin/bash
source ./config.sh
./trendSummary.sh | \
  csvcut -c 1,36,38,40,42,44,46,48,50,52,54,55,57 | \
  sed 's/avg. adult //g; s/avg. other adult/2/g; s/metabolism/metab/; s/cooperation/coop/; s/agreement/agr/g; s/pop. control/pop/; s/flirting/flirt/; s/mating/mate/; s/old age/old/' | \
  csvlook

