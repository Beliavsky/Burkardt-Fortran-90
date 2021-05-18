#! /bin/bash
#
gfortran -c -Wall test_nls_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_nls_test.o $HOME/lib/test_nls.o $HOME/lib/minpack.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_nls_test.o
#
mv a.out test_nls_test
./test_nls_test > test_nls_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_nls_test
#
echo "Normal end of execution."
