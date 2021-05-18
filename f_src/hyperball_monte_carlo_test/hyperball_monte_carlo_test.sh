#! /bin/bash
#
gfortran -c -Wall hyperball_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hyperball_monte_carlo_test.o $HOME/lib/hyperball_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hyperball_monte_carlo_test.o
#
mv a.out hyperball_monte_carlo_test
./hyperball_monte_carlo_test > hyperball_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hyperball_monte_carlo_test
#
echo "Normal end of execution."
