#! /bin/bash
#
gfortran -c -Wall disk01_quarter_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o disk01_quarter_monte_carlo_test disk01_quarter_monte_carlo_test.o $HOME/lib/disk01_quarter_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm disk01_quarter_monte_carlo_test.o
#
./disk01_quarter_monte_carlo_test > disk01_quarter_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm disk01_quarter_monte_carlo_test
#
echo "Normal end of execution."
