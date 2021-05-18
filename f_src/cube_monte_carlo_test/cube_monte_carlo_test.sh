#! /bin/bash
#
gfortran -c -Wall cube_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cube_monte_carlo_test cube_monte_carlo_test.o $HOME/lib/cube_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cube_monte_carlo_test.o
#
./cube_monte_carlo_test > cube_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cube_monte_carlo_test
#
echo "Normal end of execution."
