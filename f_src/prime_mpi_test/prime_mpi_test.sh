#! /bin/bash
#
mpirun -np 8 $HOME/bin/prime_mpi > prime_mpi_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

