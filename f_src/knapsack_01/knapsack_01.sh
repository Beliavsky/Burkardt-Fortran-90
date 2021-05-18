#! /bin/bash
#
gfortran -c -Wall knapsack_01.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv knapsack_01.o ~/lib/knapsack_01.o
#
echo "Normal end of execution."
