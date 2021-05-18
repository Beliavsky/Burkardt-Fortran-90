#! /bin/bash
#
gfortran -c -Wall big_ints_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran big_ints_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm big_ints_test.o
#
mv a.out big_ints_test
./big_ints_test > big_ints_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm big_ints_test
#
echo "Normal end of execution."
