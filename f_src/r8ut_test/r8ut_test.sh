#! /bin/bash
#
gfortran -c -Wall r8ut_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o r8ut_test r8ut_test.o /$HOME/lib/r8ut.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm r8ut_test.o
#
./r8ut_test > r8ut_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm r8ut_test
#
echo "Normal end of execution."
