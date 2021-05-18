#! /bin/bash
#
gfortran -c -Wall pyramid_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pyramid_monte_carlo_test.o $HOME/lib/pyramid_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pyramid_monte_carlo_test.o
#
mv a.out pyramid_monte_carlo_test
./pyramid_monte_carlo_test > pyramid_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pyramid_monte_carlo_test
#
echo "Normal end of execution."
