#! /bin/bash
#
gfortran -c -Wall ellipse_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ellipse_monte_carlo_test.o $HOME/lib/ellipse_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ellipse_monte_carlo_test.o
#
mv a.out ellipse_monte_carlo_test
./ellipse_monte_carlo_test > ellipse_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ellipse_monte_carlo_test
#
echo "Normal end of execution."
