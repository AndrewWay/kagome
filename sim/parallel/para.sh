rm *.o *.exe 
ls 

echo "Running in Serial"

gfortran -c 3DEFMH.f90
gfortran 3DEFMH.o -o efmh.exe
./efmh.exe

rm *.o *.exe
ls 

echo "Running in Parallel"

gfortran -fopenmp -c 3DEFMH.f90
gfortran -fopenmp 3DEFMH.o -o efmh.exe
./efmh.exe
