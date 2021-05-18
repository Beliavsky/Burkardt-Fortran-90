#! /bin/bash
#
mpifort -c -Wall ring_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort ring_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ring_mpi.o
mv a.out $HOME/bin/ring_mpi
#
echo "Normal end of execution."

