#!/bin/bash
#Extract spins from all L12 configuration files in the current directory
#Revision 2 - May 31st 2016 

rm AllSpins.txt

for f in `ls | egrep -o "gsconf[0-9]+.dat"`
do

SpinA=`cat $f | head -n 1 | tail -n 1`
SpinANeg=`cat $f | head -n 37 | tail -n 1`
SpinB=`cat $f | head -n 433 | tail -n 1`
SpinBNeg=`cat $f | head -n 469 | tail -n 1`
SpinC=`cat $f | head -n 865 | tail -n 1`
SpinCNeg=`cat $f | head -n 901 | tail -n 1`
if [[ $SpinA == "" ]];then
continue
fi

echo "$f" >> AllSpins.txt
echo "$SpinA" >> AllSpins.txt
#echo "$SpinANeg" >> AllSpins.txt
#echo "$SpinB" >> AllSpins.txt
echo "$SpinBNeg" >> AllSpins.txt
echo "$SpinC" >> AllSpins.txt
#echo "$SpinCNeg" >> AllSpins.txt

done
