#! /bin/bash
#
gfortran -c -Wall correlation_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o correlation_test correlation_test.o $HOME/lib/correlation.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm correlation_test.o
#
./correlation_test > correlation_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm correlation_test
#
echo "Normal end of execution."
