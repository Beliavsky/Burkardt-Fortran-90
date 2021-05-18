#! /bin/bash
#
mpirun -np 2 $HOME/bin/hello_mpi > hello_mpi_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

