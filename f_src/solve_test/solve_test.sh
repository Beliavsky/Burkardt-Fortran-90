#! /bin/bash
#
gfortran -c -Wall solve_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran solve_test.o $HOME/lib/solve.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm solve_test.o
#
mv a.out solve_test
./solve_test > solve_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm solve_test
#
echo "Normal end of execution."
