#! /bin/bash
#
gfortran -c -Wall circle_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o circle_monte_carlo_test circle_monte_carlo_test.o $HOME/lib/circle_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm circle_monte_carlo_test.o
#
./circle_monte_carlo_test > circle_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm circle_monte_carlo_test
#
echo "Normal end of execution."
