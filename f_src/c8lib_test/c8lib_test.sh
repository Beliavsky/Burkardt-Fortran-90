#! /bin/bash
#
gfortran -c -Wall c8lib_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o c8lib_test c8lib_test.o $HOME/lib/c8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm c8lib_test.o
#
./c8lib_test > c8lib_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm c8lib_test
#
echo "Normal end of execution."
