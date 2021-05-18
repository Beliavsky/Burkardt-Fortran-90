#! /bin/bash
#
gfortran -c -Wall triangle01_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangle01_monte_carlo_test.o $HOME/lib/triangle01_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle01_monte_carlo_test.o
#
mv a.out triangle01_monte_carlo_test
./triangle01_monte_carlo_test > triangle01_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm triangle01_monte_carlo_test
#
echo "Normal end of execution."
