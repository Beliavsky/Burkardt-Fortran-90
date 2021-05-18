#! /bin/bash
#
gfortran -c -Wall -fopenmp dijkstra_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp dijkstra_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm dijkstra_openmp.o
mv a.out $HOME/bin/dijkstra_openmp
#
echo "Normal end of execution."
