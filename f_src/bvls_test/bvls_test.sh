#! /bin/bash
#
gfortran -c -Wall bvls_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o bvls_test bvls_test.o $HOME/lib/bvls.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bvls_test.o
#
./bvls_test > bvls_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bvls_test
#
echo "Normal end of execution."
