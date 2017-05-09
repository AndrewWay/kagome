#!/bin/bash

angle_input="ground_states.dat"
output="inflections.dat"
rm $output 2> /dev/null
dirs=`ls -d */ | egrep -o "[0-9]+" | sort -n | wc -l`
t=0
echo "finding points of inflection in dataset..."
for i in `ls -d */ | egrep -o "[0-9]+" | sort -n`
do
	field_at_inflection=`~/Work/code/bash/inflectionFinder.sh $i/fort.1`
	angle_pair=`cat $angle_input | head -n $i | tail -n 1`
	if [[ !  -z  $field_at_inflection  ]];then
		echo $angle_pair $field_at_inflection >> $output
	fi
t=$((t + 1))
echo -ne "$t/$dirs directories completed\r"
done
echo -e "\ndone"
