#!/bin/bash
#Created: May 9th 2017
#Author: Andrew Way
#Description: Finds difference between two columns in two different files w.r.t. a shared column

file1=$1
file2=$2
output=$3

if [ $# -ne 3 ];then
    echo "Usage: E_diff.sh File1 File2 Output"
    exit 1
fi

rm $output

file1_length=`cat $file1 | wc -l`
file2_length=`cat $file2 | wc -l`

if [ $file1_length -ne $file2_length ];then
    echo 'File lengths not equal. Exiting.'
    exit 1
fi

for i in `seq 1 $file1_length`
do
    file1_line=`cat $file1 | head -n $i | tail -n 1`
    file2_line=`cat $file2 | head -n $i | tail -n 1`
    file1_x=`echo $file1_line | awk '{print $1}'`
    file2_x=`echo $file2_line | awk '{print $1}'`
    equality=`echo $file1_x'=='$file2_x | bc -l`
    if [ $equality -ne 1 ];then
        echo 'X values not equal. Exiting'
        exit 1
    fi
    file1_y=`echo $file1_line | awk '{print $2}'`
    file2_y=`echo $file2_line | awk '{print $2}'`
    diff=`echo "$file1_y - $file2_y" | bc -l`
    echo $file1_x $diff >> $output
done

