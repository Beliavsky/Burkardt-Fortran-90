#! /bin/bash
#
gfortran -c -Wall bvec_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o bvec_test bvec_test.o $HOME/lib/bvec.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bvec_test.o
#
./bvec_test > bvec_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bvec_test
#
echo "Normal end of execution."
