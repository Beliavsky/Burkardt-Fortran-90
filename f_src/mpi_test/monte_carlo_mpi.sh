#! /bin/bash
#
mpifort -c -Wall monte_carlo_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort monte_carlo_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm monte_carlo_mpi.o
mv a.out monte_carlo_mpi
#
mpirun -v -np 4 ./monte_carlo_mpi > monte_carlo_mpi.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm monte_carlo_mpi
#
echo "Normal end of execution."
