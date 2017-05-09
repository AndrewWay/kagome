#!/bin/bash
if [ "$(ls -A .)" ];then
echo "directory not empty: exiting"
exit
fi
echo "directory empty: initiating production"
~/Work/code/bash/createHangles.sh 0.7853981633 0.7853981633 1 0.955316618 0.955316618 1
~/Work/code/bash/generate_groundstates.sh 0 0.77 4 1.5 3.14 4 6
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
~/Work/code/bash/shotgun.sh 8 ~/Work/code/sim/3DEFMH.f90 >/dev/null
~/Work/code/bash/inflectionOperator.sh >/dev/null
cp inflections.dat ../infldata/$i"_infl.dat"
cd ..
done
