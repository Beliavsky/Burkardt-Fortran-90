#! /bin/bash
#
gfortran -c -Wall polygon_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran polygon_monte_carlo_test.o $HOME/lib/polygon_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm polygon_monte_carlo_test.o
#
mv a.out polygon_monte_carlo_test
./polygon_monte_carlo_test > polygon_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm polygon_monte_carlo_test
#
echo "Normal end of execution."
