#! /bin/bash
#
gfortran -c -Wall r8col_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o r8col_test r8col_test.o $HOME/lib/r8col.o
if [ $? -ne 0 ]; then
  echo "Load errors."
  exit
fi
rm r8col_test.o
#
./r8col_test > r8col_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm r8col_test
#
echo "Normal end of execution."
