#!/bin/bash

file1=$1
file2=$2
output="combined.txt"

length=`cat $file1 | wc -l`

rm $output

for i in `seq 1 $length`
do
    line1=`cat $file1 | head -n $i | tail -n 1`
    line2=`cat $file2 | head -n $i | tail -n 1`
    echo $line1 $line2 >> $output
done
