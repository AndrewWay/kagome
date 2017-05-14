#!/bin/bash

gsAng="ground_states.dat"
gsAngSize=`cat $gsAng | wc -l`

if [ ! -f $gsAng ];then
    echo $gsAng does not exist: exiting
exit
fi

simcode=$1
start=$2
end=$3

if [ $# != 3 ];then
    echo "usage: shotgunrun.sh <simulation-code>.f90"
    echo "exiting"
exit
fi

if [ ! -f $simcode ];then
    echo "$simcode does not exist: exiting"
exit
fi

if [ $end -gt $gsAngSize ];then
	end=$gsAngSize
fi

for i in `seq $start $end`
do
		num=`printf "%04d" $i` 
		if [ ! -f gsconf$num.dat ];then
			continue
		fi

		directory_quantity=`find * -maxdepth 0 -type d | wc -l`
		echo -ne "                                                                             \r"
		echo -ne "$i ($directory_quantity)\r"
		mkdir $i
		cp fieldAngles.dat $i/
		cp gsconf$num.dat $i/spin0000.dat
		cp $simcode $i/
		cd $i
		gfortran $simcode
		./a.out
		cd ../
done
