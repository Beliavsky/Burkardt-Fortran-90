#! /bin/bash
#
gfortran -c -Wall bdmlib_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o bdmlib_test bdmlib_test.o $HOME/lib/bdmlib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bdmlib_test.o
#
./bdmlib_test > bdmlib_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bdmlib_test
#
echo "Normal end of execution."
