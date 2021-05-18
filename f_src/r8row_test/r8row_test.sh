#! /bin/bash
#
gfortran -c -Wall r8row_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o r8row_test r8row_test.o $HOME/lib/r8row.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm r8row_test.o
#
./r8row_test > r8row_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm r8row_test
#
echo "Normal end of execution."
