#! /bin/bash
#
mpirun -np 4 $HOME/bin/communicator_mpi > communicator_mpi_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
