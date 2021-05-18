#! /bin/bash
#
gfortran -c -Wall test_interp_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o test_interp_test test_interp_test.o $HOME/lib/test_interp.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_interp_test.o
#
./test_interp_test > test_interp_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_interp_test
#
echo "Normal end of execution."
