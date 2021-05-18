#! /bin/bash
#
gfortran -c -Wall blas2_s_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o blas2_s_test blas2_s_test.o $HOME/lib/blas2_s.o $HOME/lib/blas1_s.o $HOME/lib/blas0.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm blas2_s_test.o
#
./blas2_s_test > blas2_s_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm blas2_s_test
#
echo "Normal end of execution."
