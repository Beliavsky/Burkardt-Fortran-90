#! /bin/bash
#
mpifort -c -Wall communicator_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort communicator_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm communicator_mpi.o
mv a.out $HOME/bin/communicator_mpi
#
echo "Normal end of execution."
