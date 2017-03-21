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

echo "creating ground state angles..."
~/Work/code/bash/createGSangles.sh $phlb $phub $phnum $thlb $thub $thnum
confquant=`cat ground_states.dat | wc -l`
echo "$confquant angle pairs generated"

echo "building spin files..."
for i in `seq 1 $confquant`
do
	~/Work/code/bash/angletoSpin.sh $i $L
	echo -ne "completed $i/$confquant\r"
done
echo "completed $confquant/$confquant"

echo "creating allspins.txt"
~/Work/code/bash/ExtractAllSpins.sh
echo "done"
