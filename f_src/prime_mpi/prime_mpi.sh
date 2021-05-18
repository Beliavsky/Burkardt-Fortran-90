#! /bin/bash
#
mpifort -c -Wall prime_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort prime_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm prime_mpi.o
mv a.out $HOME/bin/prime_mpi
#
echo "Normal end of execution."

