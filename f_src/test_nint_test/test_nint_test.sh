#! /bin/bash
#
gfortran -c -Wall test_nint_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_nint_test.o $HOME/lib/test_nint.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_nint_test.o
#
mv a.out test_nint_test
./test_nint_test > test_nint_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_nint_test
#
echo "Normal end of execution."
