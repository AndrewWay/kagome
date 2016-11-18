#!/bin/bash

File=ground_states.dat
L=12
fnum=`printf "%04.0f" $1`
output="conf$fnum".dat
rm $output
touch $output
echo $output
Length=`cat $File | wc -l`

val=`cat $File | head -n $1 | tail -n 1`
phi=`echo $val | awk '{print $1}'`
theta=`echo $val | awk '{print $2}'`

a=`echo "s($theta)*c($phi)" | bc -l`
b=`echo "s($theta)*s($phi)" | bc -l`
c=`echo "c($theta)" | bc -l`
d=`echo "-1*(1 - 2 * $a * $a)/(2 * $c)" | bc -l`
e=`echo "sqrt(1 - $a * $a - $d * $d)" | bc -l`

nega=`echo "-1 * $a" | bc -l`
negb=`echo "-1 * $b" | bc -l`
nege=`echo "-1 * $e" | bc -l`
negcd=`echo "-1 * $c + -1 * $d" | bc -l`

spinA="$a $b $c"
spinB="$d $e $nega"
spinC="$nege $negcd $negb"


if [[ $L == 6 ]];then
P=9
rep=3
fi

if [[ $L == 12 ]];then
P=36
rep=6
fi


length=$((3*L*L*L/4))

echo "Spin A: $spinA"
echo "Spin B: $spinB"
echo "Spin C: $spinC"

spinAx=`echo $spinA | awk '{print $1}'`
spinAy=`echo $spinA | awk '{print $2}'`
spinAz=`echo $spinA | awk '{print $3}'`
spinBx=`echo $spinB | awk '{print $1}'`
spinBy=`echo $spinB | awk '{print $2}'`
spinBz=`echo $spinB | awk '{print $3}'`
spinCx=`echo $spinC | awk '{print $1}'`
spinCy=`echo $spinC | awk '{print $2}'`
spinCz=`echo $spinC | awk '{print $3}'`

NegSpinAx=`echo "-1 * $spinAx" | bc -l`
NegSpinAy=`echo "-1 * $spinAy" | bc -l`
NegSpinAz=`echo "-1 * $spinAz" | bc -l`
NegSpinBx=`echo "-1 * $spinBx" | bc -l`
NegSpinBy=`echo "-1 * $spinBy" | bc -l`
NegSpinBz=`echo "-1 * $spinBz" | bc -l`
NegSpinCx=`echo "-1 * $spinCx" | bc -l`
NegSpinCy=`echo "-1 * $spinCy" | bc -l`
NegSpinCz=`echo "-1 * $spinCz" | bc -l`

negspinA="$NegSpinAx $NegSpinAy $NegSpinAz"
negspinB="$NegSpinBx $NegSpinBy $NegSpinBz"
negspinC="$NegSpinCx $NegSpinCy $NegSpinCz"


for i in `seq 1 $rep`
do
for j in `seq 1 $P`
do
echo $spinA >> $output
done
for k in `seq 1 $P`
do
echo $negspinA >> $output
done
done

for i in `seq 1 $rep`
do
for j in `seq 1 $P`
do
echo $spinB >> $output
done
for k in `seq 1 $P`
do
echo $negspinB >> $output
done
done

for i in `seq 1 $rep`
do
for j in `seq 1 $P`
do
echo $spinC >> $output
done
for k in `seq 1 $P`
do
echo $negspinC >> $output
done
done

echo "Completed"

