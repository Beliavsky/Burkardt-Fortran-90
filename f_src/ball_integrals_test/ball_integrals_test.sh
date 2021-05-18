#! /bin/bash
#
gfortran -c -Wall ball_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o ball_integrals_test ball_integrals_test.o $HOME/lib/ball_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ball_integrals_test.o
#
./ball_integrals_test > ball_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ball_integrals_test
#
echo "Normal end of execution."
