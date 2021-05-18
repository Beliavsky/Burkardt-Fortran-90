#! /bin/bash
#
gfortran -c -Wall test_lls_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_lls_test.o $HOME/lib/test_lls.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_lls_test.o
#
mv a.out test_lls_test
./test_lls_test > test_lls_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_lls_test
#
echo "Normal end of execution."
