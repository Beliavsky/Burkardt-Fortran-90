#! /bin/bash
#
gfortran -c -Wall errors_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o errors_test errors_test.o $HOME/lib/errors.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm errors_test.o
#
./errors_test > errors_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm errors_test
#
echo "Normal end of execution."
