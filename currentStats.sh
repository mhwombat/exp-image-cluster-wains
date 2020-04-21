#!/bin/sh
grep 'Summary - avg' ${log} | tail -n 1 | sed 's/,/\n/g'
