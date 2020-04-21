#!/bin/bash

imageDir=/home/amy/mnist/testDataWithAnomalies

convert 'canvas:#ffffff[28x28]' default.jpeg

makeFilename()
{
  label=$1
  echo ${label} | sed 's/(//; s/)//; s/,/_/'
}

selectImages()
{
  label=$1
  grep " agrees .* Image" $log | grep "${label}" | sed "s@.*Image @${imageDir}/@; s/ has label.*//"
}

averageImages()
{
  label=$1
  filename=`makeFilename $1`
  shift
  convert $* -evaluate-sequence mean avg_${filename}.jpeg
}

makeAverageImageForLabel()
{
  label=$1
  images=`selectImages ${label}`
  averageImages ${label} ${images} default.jpeg
}

# calcStatsForLabel()
# {
#   label=$1
#   filename=`makeFilename $1`
#   grep " agrees .* Image" $log | grep "${label}" | sed "s@.*Image @@; s/.jpeg has label.*//" | sort -u | sed "s/$/,${filename}/" > temp
#   join -t, --header temp ${answers} | csvcut -c 2,1,15,16,17 > stats_${filename}.csv
# }

analyseLabel()
{
  label=$1
  makeAverageImageForLabel $1
  # calcStatsForLabel $1
}

analyseLabel 0
analyseLabel 1
analyseLabel 2
analyseLabel 3
analyseLabel 4
analyseLabel 5
analyseLabel 6
analyseLabel 7
analyseLabel 8
analyseLabel 9
analyseLabel 10
analyseLabel 11
analyseLabel 12
analyseLabel 13
analyseLabel 14
analyseLabel 15
analyseLabel 16
analyseLabel 17
analyseLabel 18
analyseLabel 19
