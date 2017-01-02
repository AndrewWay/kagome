#rm *.o *.exe 
#ls 

echo "Running in Serial"

gfortran -c 3DEFMH.f90
gfortran 3DEFMH.o -o efmh.exe
./efmh.exe

rm *.o *.exe
#ls 

echo "Running in Parallel"

#DEBUGGING COMPILATION
#gfortran -g3 -fcheck=all -Wall -fbacktrace -fsanitize=undefined -fsanitize=address -fsanitize=leak -fopenmp -c 3DEFMH.f90
#gfortran -g3 -fcheck=all -Wall -fbacktrace -fsanitize=undefined -fsanitize=address -fsanitize=leak -fopenmp 3DEFMH.o -o efmh.exe

gfortran -fopenmp -c 3DEFMH.f90
gfortran -fopenmp 3DEFMH.o -o efmh.exe
./efmh.exe
