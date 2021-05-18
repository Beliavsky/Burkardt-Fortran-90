#! /bin/bash
#
gfortran -c -Wall ubvec_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ubvec_test.o $HOME/lib/ubvec.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ubvec_test.o
#
mv a.out ubvec_test
./ubvec_test > ubvec_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ubvec_test
#
echo "Normal end of execution."
