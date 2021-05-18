#! /bin/bash
#
gfortran -c -Wall rk4_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o rk4_test rk4_test.o $HOME/lib/rk4.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm rk4_test.o
#
./rk4_test > rk4_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm rk4_test
#
gnuplot < predator_commands.txt
#
echo "Normal end of execution."
