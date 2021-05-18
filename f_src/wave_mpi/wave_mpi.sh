#! /bin/bash
#
mpifort -c -Wall wave_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort wave_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm wave_mpi.o
mv a.out $HOME/bin/wave_mpi
#
echo "Normal end of execution."

