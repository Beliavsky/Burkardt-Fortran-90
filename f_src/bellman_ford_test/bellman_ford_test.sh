#! /bin/bash
#
gfortran -c -Wall bellman_ford_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o bellman_ford_test bellman_ford_test.o $HOME/lib/bellman_ford.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bellman_ford_test.o
#
./bellman_ford_test > bellman_ford_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bellman_ford_test
#
echo "Normal end of execution."
