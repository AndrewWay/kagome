#!/bin/bash

Hth_lb=-1
Hth_ub=1
n=10
phi=0.785398
thinc=`echo "( $Hth_ub - $Hth_lb ) / $n" | bc -l`
theta=$Hth_lb
for i in `seq 1 n`
do
mkdir $i
cp gs* $i/
cd $i/
shotgun.sh 8 3DEFMH.f90
theta=`echo "$theta + $thinc" | bc -l`
done
