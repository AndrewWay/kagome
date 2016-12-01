#!/bin/bash

phlb=$1
phub=$2
phnum=$3
thlb=$4
thub=$5
thnum=$6

output="ground_states.dat"
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
    for j in `seq 0 $phnum`
    do
        phi=`echo "$phlb + $j * $phinc" | bc -l`
		a=`echo "s($theta)*c($phi)" | bc -l`
		b=`echo "s($theta)*s($phi)" | bc -l`
		c=`echo "c($theta)" | bc -l`
		d=`echo "-1*(1 - 2 * $a * $a)/(2 * $c)" | bc -l`
		forbiddenCheck=`echo "1 - $a * $a - $d * $d" | bc -l`

		if [[ `echo $forbiddenCheck'>'0 | bc -l` == 1 ]]; then
e=`echo "sqrt(1 - $a * $a - $d * $d)" | bc -l`        	
			echo $phi $theta >> $output     
		fi

done 
done
