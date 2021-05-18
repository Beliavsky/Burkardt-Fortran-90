#! /bin/bash
#
gfortran -c -Wall bellman_ford.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv bellman_ford.o ~/lib/bellman_ford.o
#
echo "Normal end of execution."
