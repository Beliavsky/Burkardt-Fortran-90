#! /bin/bash
#
gfortran -c -Wall ieee_uniform_sample_test.f90
if [ $? -ne 0 ]; then
    echo "Compile error."
  exit
fi
#
gfortran ieee_uniform_sample_test.o $HOME/lib/ieee_uniform_sample.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ieee_uniform_sample_test.o
#
mv a.out ieee_uniform_sample_test
./ieee_uniform_sample_test > ieee_uniform_sample_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ieee_uniform_sample_test
#
echo "Normal end of execution."
