#! /bin/bash
#
gfortran -c -Wall partition_problem.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv partition_problem.o ~/lib/partition_problem.o
#
echo "Normal end of execution."
