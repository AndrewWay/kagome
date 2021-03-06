#!/bin/bash

phlb=$1
phub=$2
phnum=$3
thlb=$4
thub=$5
thnum=$6

output="field_angles.dat"
rm $output 2> /dev/null

thinc=`echo "($thub - $thlb)/$thnum" | bc -l`
phinc=`echo "($phub - $phlb)/$phnum" | bc -l`

if [ `echo $thinc'<'0 | bc -l` == 1 ];then
thinc=`echo -1*$thinc | bc -l`
fi

if [ `echo $phinc'<'0 | bc -l` == 1 ];then
phinc=`echo -1*$phinc | bc -l`
fi

for i in `seq 0 $thnum`
do
theta=`echo "$thlb + $i * $thinc" | bc -l`
    for j in `seq 1 $phnum`
    do
        phi=`echo "$phlb + $j * $phinc" | bc -l`
			echo $phi $theta >> $output     
done 
done
