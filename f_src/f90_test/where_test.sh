#! /bin/bash
#
gfortran -c -Wall where_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran where_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm where_test.o
#
mv a.out where_test
./where_test > where_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm where_test
#
echo "Normal end of execution."
