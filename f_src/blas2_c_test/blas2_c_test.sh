#! /bin/bash
#
gfortran -c -Wall blas2_c_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o blas2_c_test blas2_c_test.o $HOME/lib/blas2_c.o $HOME/lib/blas1_c.o $HOME/lib/blas0.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm blas2_c_test.o
#
./blas2_c_test > blas2_c_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm blas2_c_test
#
echo "Normal end of execution."
