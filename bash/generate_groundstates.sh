#!/bin/bash

thlb=$1
thub=$2
thnum=$3
phlb=$4
phub=$5
phnum=$6


./createGSangles.sh $thlb $thub $thnum $phlb $phub $phnum

confquant = `echo "$thnum * $phnum" | bc -l`

for i in `seq 1 $confquant`
do
./angletoSpin.sh $i
done
