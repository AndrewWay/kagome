#!/bin/bash
if [ "$(ls -A .)" ];then
echo "directory not empty: exiting"
exit
fi
echo "directory empty: initiating production"
createHangles.sh 0.78 0.78 1 0 3.14 100
generate_groundstates.sh -1 1 20 0 3 30 2
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
cd $i/
shotgun.sh 8 ~/Desktop/Work/code/sim/3DEFMH.f90 >/dev/null
inflectionOperator.sh >/dev/null
cp inflections.dat ../infldata/$i"_infl.dat"
cd ..
done
