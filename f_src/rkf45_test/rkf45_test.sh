#! /bin/bash
#
gfortran -c -Wall rkf45_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran rkf45_test.o $HOME/lib/rkf45.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm rkf45_test.o
#
mv a.out rkf45_test
./rkf45_test > rkf45_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm rkf45_test
#
echo "Normal end of execution."
