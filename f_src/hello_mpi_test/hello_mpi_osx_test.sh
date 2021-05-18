#! /bin/bash
#
mpirun -np 4 --hostfile hostfile.txt $HOME/bin/hello_mpi > hello_mpi_osx_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

