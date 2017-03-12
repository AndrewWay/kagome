#!/bin/bash
sim_code=`cat settings.txt | grep "sim_code" | awk '{print $2}'`
parameters_filesize=`cat parameters.txt | wc -l`
database=`cat settings.txt | grep "database" | awk '{print $2}'`
current_dir=`pwd`
echo "USING $sim_code"
if [ ! -d "$database" ];then
echo "MAKING $database"
    mkdir $database
fi
dir=$database
for i in `seq 1 $parameters_filesize`
do
    line=`cat parameters.txt | head -n $i | tail -n 1`
    parameter=`echo $line | awk '{print $1}'`
    value=`echo $line | awk '{print $2}'`
    folder="$parameter$value"
    echo "$folder"
    dir=$dir/$folder
    if [ ! -d "$dir" ];then
        echo "MAKING $dir"
        mkdir $dir
    fi
done

#Move code contents into the working directory
if [ ! -f "$sim_code" ];then
    echo "$sim_code does not exist"
    exit 1
fi
cp $sim_code $dir
#change to dir
#cd $dir
#compile
#gfortran $sim_code -o sim.exe
#run
echo "RUNNING SIM.EXE"
#./sim.exe &
#cd $current_dir
