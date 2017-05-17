#!/bin/bash

for i in `ls -d */`
do
cd $i
gfortran 3DEFMH.f90
nohup ./a.out &
cd ..
done
