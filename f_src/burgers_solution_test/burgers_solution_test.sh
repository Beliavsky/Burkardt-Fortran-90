#! /bin/bash
#
gfortran -c -Wall burgers_solution_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o burgers_solution_test burgers_solution_test.o $HOME/lib/burgers_solution.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm burgers_solution_test.o
#
./burgers_solution_test > burgers_solution_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm burgers_solution_test
#
echo "Normal end of execution."
