#! /bin/bash
#
gfortran -c -Wall fn_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fn_test.o $HOME/lib/fn.o $HOME/lib/test_values.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fn_test.o
#
mv a.out fn_test
./fn_test > fn_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fn_test
#
echo "Normal end of execution."
