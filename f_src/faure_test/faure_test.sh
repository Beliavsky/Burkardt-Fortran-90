#! /bin/bash
#
gfortran -c -Wall faure_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran faure_test.o $HOME/lib/faure.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm faure_test.o
#
mv a.out faure_test
./faure_test > faure_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm faure_test
#
echo "Normal end of execution."
