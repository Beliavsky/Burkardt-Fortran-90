#! /bin/bash
#
mpifort -c -Wall satisfy_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort satisfy_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm satisfy_mpi.o
mv a.out $HOME/bin/satisfy_mpi
#
echo "Normal end of execution."

