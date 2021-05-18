#! /bin/bash
#
gfortran -c -Wall hypercube_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hypercube_monte_carlo_test.o $HOME/lib/hypercube_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hypercube_monte_carlo_test.o
#
mv a.out hypercube_monte_carlo_test
./hypercube_monte_carlo_test > hypercube_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hypercube_monte_carlo_test
#
echo "Normal end of execution."
