#!/bin/bash

output="Hvs_degStates.txt"
rm $output

echo "HPHI    HTHE    SPHI    STHE" > $output

for i in `seq 1 110`
do
h_line=`cat field_angles.dat | head -n $i | tail -n 1`
hphi=`echo $h_line | awk '{print $1}'`
hthe=`echo $h_line | awk '{print $2}'`



done
