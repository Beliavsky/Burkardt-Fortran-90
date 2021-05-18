#! /bin/bash
#
gfortran -c -Wall allocatable_array_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran allocatable_array_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm allocatable_array_test.o
#
mv a.out allocatable_array_test
./allocatable_array_test > allocatable_array_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm allocatable_array_test
#
echo "Normal end of execution."
