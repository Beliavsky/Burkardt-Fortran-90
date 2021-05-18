#! /bin/bash
#
mpifort -c -Wall quad_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort quad_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quad_mpi.o
mv a.out $HOME/bin/quad_mpi
#
echo "Normal end of execution."

