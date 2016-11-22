#!/bin/bash

phlb=$1
phub=$2
phnum=$3
thlb=$4
thub=$5
thnum=$6
L=$7
#rm gsconf*

if [[ $# != 7 ]];then
echo "Required arguments: phiLowerBound phiUpperBound phiQuantity thetaLowerBound thetaUpperBound thetaQuantity latticeDimension"
exit 1
fi

./createGSangles.sh $phlb $phub $phnum $thlb $thub $thnum

confquant=`echo "$thnum * $phnum" | bc -l`

for i in `seq 1 $confquant`
do
./angletoSpin.sh $i $L
done

./ExtractAllSpins.sh
