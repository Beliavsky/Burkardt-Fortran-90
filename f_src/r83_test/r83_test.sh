#! /bin/bash
#
gfortran -c -Wall r83_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o r83_test r83_test.o $HOME/lib/r83.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm r83_test.o
#
./r83_test > r83_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm r83_test
#
echo "Normal end of execution."
