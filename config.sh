#!/bin/sh
export imagedir=`grep imageDir iomha.config | sed 's/.*="//; s/"$//'`
export alifedir=`grep workingDir iomha.config | sed 's/.*="//; s/"$//'`
export logdir=${alifedir}/log
export logname=`grep experimentName iomha.config | sed 's/.*="//; s/"$//'`.log
export log=${logdir}/${logname}
export statsname=rawStats
export stats=${alifedir}/${statsname}
