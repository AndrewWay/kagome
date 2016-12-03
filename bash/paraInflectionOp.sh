#!/bin/bash

angle_input="ground_states.dat"
output="inflections.dat"
rm $output
dirs=`ls -d */ | egrep -o "[0-9]+" | sort -n | wc -l`
t=0
echo "finding points of inflection in dataset..."
for i in `ls -d */ | egrep -o "[0-9]+" | sort -n`
do
	field_at_inflection=`inflectionFinder.sh $i/fort.1`
	angle_pair=`cat $angle_input | head -n $i | tail -n 1`
echo $angle_pair $field_at_inflection >> $output
t=$((t + 1))
echo -ne "$t/$dirs directories completed\r"
done
echo -e "\ndone"
