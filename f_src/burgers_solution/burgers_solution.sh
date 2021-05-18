#! /bin/bash
#
gfortran -c -Wall burgers_solution.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv burgers_solution.o ~/lib/burgers_solution.o
#
echo "Normal end of execution."
