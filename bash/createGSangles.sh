#!/bin/bash

thlb=$1
thub=$2
thnum=$3
phlb=$4
phub=$5
phnum=$6

output="ground_states.dat"
rm $output

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
th=`echo "$thlb + $i * $thinc" | bc -l`
    for j in `seq 0 $phnum`
    do
        ph=`echo "$phlb + $j * $phinc" | bc -l`
        echo $th $ph >> $output
done 
done
