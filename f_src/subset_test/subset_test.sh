#! /bin/bash
#
gfortran -c -Wall subset_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran subset_test.o /$HOME/lib/subset.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm subset_test.o
#
mv a.out subset_test
./subset_test > subset_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm subset_test
#
echo "Normal end of execution."
