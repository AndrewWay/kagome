#!/bin/bash
sim_code=`cat settings.txt | grep "sim_code" | awk '{print $2}'`
parameters_filesize=`cat parameters.txt | wc -l`
database=`cat settings.txt | grep "database" | awk '{print $2}'`
job_info=`cat settings.txt | grep "job_info" | awk '{print $2}'`
current_dir=`pwd`
preexisting="NO"
if [ ! -d "$database" ];then
    mkdir $database
fi
jobstat=$sim_code
dir=$database
for i in `seq 1 $parameters_filesize`
do
    line=`cat parameters.txt | head -n $i | tail -n 1`
    parameter=`echo $line | awk '{print $1}'`
    value=`echo $line | awk '{print $2}'`
    folder="$parameter$value"
    jobstat="$jobstat $folder"
    dir=$dir/$folder
    if [ ! -d "$dir" ];then
	preexisting="YES"
        mkdir $dir
    fi
done

#Move code contents into the working directory
if [ ! -f "$sim_code" ];then
    exit 1
fi
if [ ! $preexisting == "YES" ];then
    exit 2
else
echo $jobstat "RUNNING" >> $job_info
cp $sim_code $dir
#change to dir
cd $dir
#compile
#gfortran $sim_code -o sim.exe
#run
#./sim.exe &
cd $current_dir
fi
