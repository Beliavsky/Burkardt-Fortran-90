#! /bin/bash
#
gfortran -c -Wall square_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran square_monte_carlo_test.o $HOME/lib/square_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm square_monte_carlo_test.o
#
mv a.out square_monte_carlo_test
./square_monte_carlo_test > square_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm square_monte_carlo_test
#
echo "Normal end of execution."
