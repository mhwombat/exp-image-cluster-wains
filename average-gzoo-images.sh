#!/bin/bash

imageDir=/home/amy/GalaxyZoo/table2/tiny-images
answers=/home/amy/GalaxyZoo/GalaxyZoo1_DR_table2_sorted.csv
convert 'canvas:#808080[21x21]' default.jpeg

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
#   join -t, temp ${answers} | csvcut -c 2,1,15,16,17 > stats_${filename}.csv
# }

analyseLabel()
{
  label=$1
  makeAverageImageForLabel $1
  # calcStatsForLabel $1
}

analyseLabel "(-3,3)"
analyseLabel "(-2,3)"
analyseLabel "(-1,3)"
analyseLabel "(0,3)"

analyseLabel "(-3,2)"
analyseLabel "(-2,2)"
analyseLabel "(-1,2)"
analyseLabel "(0,2)"
analyseLabel "(1,2)"

analyseLabel "(-3,1)"
analyseLabel "(-2,1)"
analyseLabel "(-1,1)"
analyseLabel "(0,1)"
analyseLabel "(1,1)"
analyseLabel "(2,1)"

analyseLabel "(-3,0)"
analyseLabel "(-2,0)"
analyseLabel "(-1,0)"
analyseLabel "(0,0)"
analyseLabel "(1,0)"
analyseLabel "(2,0)"
analyseLabel "(3,0)"

analyseLabel "(-2,-1)"
analyseLabel "(-1,-1)"
analyseLabel "(0,-1)"
analyseLabel "(1,-1)"
analyseLabel "(2,-1)"
analyseLabel "(3,-1)"

analyseLabel "(-1,-2)"
analyseLabel "(0,-2)"
analyseLabel "(1,-2)"
analyseLabel "(2,-2)"
analyseLabel "(3,-2)"

analyseLabel "(0,-3)"
analyseLabel "(1,-3)"
analyseLabel "(2,-3)"
analyseLabel "(3,-3)"

