#! /bin/bash
#
gfortran -c -Wall ball_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o ball_monte_carlo_test ball_monte_carlo_test.o $HOME/lib/ball_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ball_monte_carlo_test.o
#
./ball_monte_carlo_test > ball_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ball_monte_carlo_test
#
echo "Normal end of execution."
