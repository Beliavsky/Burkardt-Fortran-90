#! /bin/bash
#
gfortran -c -Wall cuda_loop_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cuda_loop_test cuda_loop_test.o $HOME/lib/cuda_loop.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cuda_loop_test.o
#
./cuda_loop_test > cuda_loop_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cuda_loop_test
#
echo "Normal end of execution."
