#! /bin/bash
#
gfortran -c -Wall annulus_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o annulus_monte_carlo_test annulus_monte_carlo_test.o $HOME/lib/annulus_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm annulus_monte_carlo_test.o
#
./annulus_monte_carlo_test > annulus_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm annulus_monte_carlo_test
#
echo "Normal end of execution."
