#! /bin/bash
#
gfortran -c -Wall test_int_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_int_test.o $HOME/lib/test_int.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_int_test.o
#
mv a.out test_int_test
./test_int_test > test_int_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_int_test
#
echo "Normal end of execution."
