#! /bin/bash
#
gfortran -c -Wall test_int_2d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_int_2d_test.o $HOME/lib/test_int_2d.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_int_2d_test.o
#
mv a.out test_int_2d_test
./test_int_2d_test > test_int_2d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_int_2d_test
#
echo "Normal end of execution."
