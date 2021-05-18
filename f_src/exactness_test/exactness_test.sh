#! /bin/bash
#
gfortran -c -Wall exactness_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran exactness_test.o $HOME/lib/exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm exactness_test.o
#
mv a.out exactness_test
./exactness_test > exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm exactness_test
#
echo "Normal end of execution."
