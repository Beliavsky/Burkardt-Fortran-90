#! /bin/bash
#
gfortran -c -Wall byname_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o byname_test byname_test.o /$HOME/lib/byname.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm byname_test.o
#
./byname_test > byname_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm byname_test
#
echo "Normal end of execution."
