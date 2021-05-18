#! /bin/bash
#
gfortran -c -Wall truncated_normal_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran truncated_normal_test.o $HOME/lib/truncated_normal.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm truncated_normal_test.o
#
mv a.out truncated_normal_test
./truncated_normal_test > truncated_normal_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm truncated_normal_test
#
echo "Normal end of execution."
