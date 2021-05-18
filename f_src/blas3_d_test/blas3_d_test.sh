#! /bin/bash
#
gfortran -c -Wall blas3_d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o blas3_d_test blas3_d_test.o $HOME/lib/blas3_d.o $HOME/lib/blas2_d.o $HOME/lib/blas1_d.o $HOME/lib/blas0.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm blas3_d_test.o
#
./blas3_d_test > blas3_d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm blas3_d_test
#
echo "Normal end of execution."
