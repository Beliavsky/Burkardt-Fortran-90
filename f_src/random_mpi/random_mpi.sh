#! /bin/bash
#
mpifort -c -Wall random_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort random_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm random_mpi.o
mv a.out $HOME/bin/random_mpi
#
echo "Normal end of execution."

