#! /bin/bash
#
gfortran -c -Wall wedge_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran wedge_monte_carlo_test.o $HOME/lib/wedge_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm wedge_monte_carlo_test.o
#
mv a.out wedge_monte_carlo_test
./wedge_monte_carlo_test > wedge_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm wedge_monte_carlo_test
#
echo "Normal end of execution."
