#!/bin/bash

phlb=$1
phub=$2
phnum=$3
thlb=$4
thub=$5
thnum=$6


./createGSangles.sh $phlb $phub $phnum $thlb $thub $thnum

confquant=`echo "$thnum * $phnum" | bc -l`

for i in `seq 1 $confquant`
do
./angletoSpin.sh $i
done
