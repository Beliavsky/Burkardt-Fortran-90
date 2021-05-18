#! /bin/bash
#
gfortran -c -Wall sort_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sort_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sort_test.o
#
mv a.out sort_test
./sort_test > sort_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sort_test
#
echo "Normal end of execution."
