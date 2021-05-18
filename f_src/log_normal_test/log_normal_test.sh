#! /bin/bash
#
gfortran -c -Wall log_normal_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran log_normal_test.o $HOME/lib/log_normal.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm log_normal_test.o
#
mv a.out log_normal_test
./log_normal_test > log_normal_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm log_normal_test
#
echo "Normal end of execution."
