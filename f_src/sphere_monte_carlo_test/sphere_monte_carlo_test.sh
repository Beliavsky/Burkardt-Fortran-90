#! /bin/bash
#
gfortran -c -Wall sphere_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sphere_monte_carlo_test.o $HOME/lib/sphere_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_monte_carlo_test.o
#
mv a.out sphere_monte_carlo_test
./sphere_monte_carlo_test > sphere_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sphere_monte_carlo_test
#
echo "Normal end of execution."
