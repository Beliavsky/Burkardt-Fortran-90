#! /bin/bash
#
gfortran -c -Wall random_sorted_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o random_sorted_test random_sorted_test.o \
  $HOME/lib/random_sorted.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm random_sorted_test.o
#
./random_sorted_test > random_sorted_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm random_sorted_test
#
echo "Normal end of execution."
