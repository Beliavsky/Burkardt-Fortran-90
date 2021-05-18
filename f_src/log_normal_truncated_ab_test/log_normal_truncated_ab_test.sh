#! /bin/bash
#
gfortran -c -Wall log_normal_truncated_ab_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o log_normal_truncated_ab_test log_normal_truncated_ab_test.o \
  $HOME/lib/log_normal_truncated_ab.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm log_normal_truncated_ab_test.o
#
./log_normal_truncated_ab_test > log_normal_truncated_ab_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm log_normal_truncated_ab_test
#
echo "Normal end of execution."
