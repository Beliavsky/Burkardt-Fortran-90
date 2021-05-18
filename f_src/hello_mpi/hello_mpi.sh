#! /bin/bash
#
mpifort -c -Wall hello_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort hello_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hello_mpi.o
mv a.out $HOME/bin/hello_mpi
#
echo "Normal end of execution."

