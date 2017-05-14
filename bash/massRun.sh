#!/bin/bash

num_of_threads=8 #Number of threads used for creating ground states and executing simulation code

#check if directory empty before filling with data
if [ "$(ls -A .)" ];then
    echo "directory not empty: exiting"
exit
fi
echo "directory empty: initiating production"

#create the field angles
createHangles.sh 0.7853981633 0.7853981633 1 0.955316618 0.955316618 1
#create the ground state configuration files
generate_groundstates.sh 0 0.77 4 1.5 3.14 4 6

#length is the number of different fields there are
length=`cat field_angles.dat | wc -l`

mkdir infldata
for i in `seq 1 $length`
do
    mkdir $i
    cp gs* $i/
    cp ground_states.dat $i/
    Hpair=`cat field_angles.dat | head -n $i | tail -n 1`
    echo $Hpair > $i/fieldAngles.dat
    echo -ne "$i/$length\r"
    echo 'hello'
    cd $i/
    #What does shotgun.sh do?
    shotgun.sh $num_of_threads ~/Work/code/sim/3DEFMH.f90 >/dev/null
    #inflectionOperator.sh >/dev/null
    #cp inflections.dat ../infldata/$i"_infl.dat"
    cd ..
done
