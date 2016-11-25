#!/bin/bash

gsAng="ground_states.dat"

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

if [ ! -f ~/Desktop/Work/code/sim/$simcode ];then
echo "$simcode does not exist: exiting"
exit
fi

dir_name_precision=6


for i in `seq $start $end`
do
		num=`printf "%04d" $i` 
		if [ ! -f gsconf$num.dat ];then
			echo "gsconf$num.dat does not exist: aborting"
			continue
		fi
	line=`cat $gsAng | head -n $i | tail -n 1`
	theta=`echo $line | awk '{print $1}' | cut -c 1-$dir_name_precision`
	phi=`echo $line | awk '{print $2}' | cut -c 1-$dir_name_precision`

	run_dir="$theta"_"$phi"
	run_dir=`echo $run_dir | tr - neg`
	if [ -d "$run_dir" ]; then
		i=$((i - 1))
		echo "Directory already exists: increasing directory name precision"
		dir_name_precision=$((dir_name_precision + 1))
	else
		echo "$run_dir ($i/$end)"
		mkdir $run_dir
		cp gsconf$num.dat $run_dir/spin0000.dat
		cp ~/Desktop/Work/code/sim/$simcode $run_dir/
		cd $run_dir
		gfortran $simcode
		./a.out
		cd ../
	fi
done
