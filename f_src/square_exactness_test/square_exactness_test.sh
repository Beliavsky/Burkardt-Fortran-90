#! /bin/bash
#
gfortran -c -Wall square_exactness_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran square_exactness_test.o $HOME/lib/square_exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm square_exactness_test.o
#
mv a.out square_exactness_test
./square_exactness_test > square_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm square_exactness_test
#
echo "Normal end of execution."
