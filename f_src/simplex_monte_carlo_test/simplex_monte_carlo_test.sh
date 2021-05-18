#! /bin/bash
#
gfortran -c -Wall simplex_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran simplex_monte_carlo_test.o $HOME/lib/simplex_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm simplex_monte_carlo_test.o
#
mv a.out simplex_monte_carlo_test
./simplex_monte_carlo_test > simplex_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm simplex_monte_carlo_test
#
echo "Normal end of execution."
