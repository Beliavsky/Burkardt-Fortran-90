#! /bin/bash
#
mpifort -c -Wall search_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort search_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm search_mpi.o
mv a.out $HOME/bin/search_mpi
#
echo "Normal end of execution."

