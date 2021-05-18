#! /bin/bash
#
gfortran -c -Wall walker_sample_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o walker_sample_test walker_sample_test.o $HOME/lib/walker_sample.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm walker_sample_test.o
#
./walker_sample_test > walker_sample_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm walker_sample_test
#
echo "Normal end of execution."
