#!/bin/bash
#Extract spins from all L12 configuration files
#Revision 2 - May 31st 2016 

f="conf010200.dat"
rm AllSpins.txt
echo `date` >> AllSpins.txt

for i in `ls -d */ | egrep -o "[0-9]+"`
do
for j in `echo {00..29}`
do
echo "Extracting from $i $j"
SpinA=`cat $i/$j/$f | head -n 1 | tail -n 1`
SpinANeg=`cat $i/$j/$f | head -n 37 | tail -n 1`
SpinB=`cat $i/$j/$f | head -n 433 | tail -n 1`
SpinBNeg=`cat $i/$j/$f | head -n 469 | tail -n 1`
SpinC=`cat $i/$j/$f | head -n 865 | tail -n 1`
SpinCNeg=`cat $i/$j/$f | head -n 901 | tail -n 1`

#echo `echo "$SpinA - $SpinANeg" | bc -l` >> tmpASpinDiff.txt
#echo `echo "$SpinB - $SpinBNeg" | bc -l` >> tmpBSpinDiff.txt
#echo `echo "$SpinC - $SpinCNeg" | bc -l` >> tmpCSpinDiff.txt

echo "$i_$j" >> AllSpins.txt
echo "$SpinA" >> AllSpins.txt
#echo "$SpinANeg" >> AllSpins.txt
#echo "$SpinB" >> AllSpins.txt
echo "$SpinBNeg" >> AllSpins.txt
echo "$SpinC" >> AllSpins.txt
#echo "$SpinCNeg" >> AllSpins.txt
#echo "1 1 1" >> AllSpins.txt
done
done
