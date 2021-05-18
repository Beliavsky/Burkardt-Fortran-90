#! /bin/bash
#
mpifort -c -Wall type_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort type_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm type_mpi.o
mv a.out type_mpi
#
mpirun -v -np 4 ./type_mpi > type_mpi.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm type_mpi
#
echo "Normal end of execution."
