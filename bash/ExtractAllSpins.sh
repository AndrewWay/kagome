#!/bin/bash
#Extract spins from all L12 configuration files in the current directory
#Revision 2 - May 31st 2016 

rm AllSpins.txt 2> /dev/null

for i in `echo conf*`
do
f=$i
length=`cat $f | wc -l`
if [ $length -eq 6 ];then
    s1=1
    s2=2
    s3=3
    s4=4
    s5=5
    s6=6
elif [ $length -eq 162 ];then
    s1=1
    s2=10
    s3=55
    s4=64
    s5=109
    s6=118
elif [ $length -eq 1296 ];then
    s1=1
    s2=37
    s3=433
    s4=469
    s5=865
    s6=901
else
    echo "Cannot recognize lattice dimension"
    echo "Exiting"
    exit 1
fi
SpinA=`cat $f | head -n $s1 | tail -n 1`
SpinANeg=`cat $f | head -n $s2 | tail -n 1`
SpinB=`cat $f | head -n $s3 | tail -n 1`
SpinBNeg=`cat $f | head -n $s4 | tail -n 1`
SpinC=`cat $f | head -n $s5 | tail -n 1`
SpinCNeg=`cat $f | head -n $s6 | tail -n 1`
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
