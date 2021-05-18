#! /bin/bash
#
gfortran -c -Wall cyclic_reduction_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cyclic_reduction_test cyclic_reduction_test.o $HOME/lib/cyclic_reduction.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cyclic_reduction_test.o
#
./cyclic_reduction_test > cyclic_reduction_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cyclic_reduction_test
#
echo "Normal end of execution."
