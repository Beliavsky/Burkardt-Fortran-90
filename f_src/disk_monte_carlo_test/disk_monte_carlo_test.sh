#! /bin/bash
#
gfortran -c -Wall disk_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o disk_monte_carlo_test disk_monte_carlo_test.o $HOME/lib/disk_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm disk_monte_carlo_test.o
#
./disk_monte_carlo_test > disk_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm disk_monte_carlo_test
#
echo "Normal end of execution."
