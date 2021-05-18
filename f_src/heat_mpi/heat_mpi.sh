#! /bin/bash
#
mpifort -c -Wall heat_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort heat_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm heat_mpi.o
mv a.out $HOME/bin/heat_mpi
#
echo "Normal end of execution."

