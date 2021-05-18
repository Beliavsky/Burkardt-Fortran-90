#! /bin/bash
#
gfortran -c -Wall ellipsoid_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o ellipsoid_monte_carlo_test ellipsoid_monte_carlo_test.o $HOME/lib/ellipsoid_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ellipsoid_monte_carlo_test.o
#
./ellipsoid_monte_carlo_test > ellipsoid_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ellipsoid_monte_carlo_test
#
echo "Normal end of execution."
