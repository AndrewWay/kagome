#!/bin/bash

cores=$1
simcode=$2

if [[ "$cores" -eq "" ]];then
echo "Number of cores unspecified: assuming 1 core"
cores=1
fi

gsAng="ground_states.dat"

if [ ! -f $gsAng ];then
echo $gsAng does not exist: exiting
exit
fi

dirQuant=`cat $gsAng | wc -l`

jobsperCore=`echo $dirQuant / $cores | bc -l`

jobsperCore=`echo ${jobsperCore%.*}`
jobsperCore=$((jobsperCore + 1))
echo $jobsperCore
start=1
end=$jobsperCore
for i in `seq 1 $cores`
do
echo "Jobs $start through $end submitted on core $i"
shotgunload.sh $simcode $start $end &
start=$(( end + 1 ))
end=`echo $end + $jobsperCore | bc -l`
done
