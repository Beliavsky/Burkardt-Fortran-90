#! /bin/bash
#
gfortran -c -Wall cvt_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cvt_test cvt_test.o $HOME/lib/cvt.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cvt_test.o
#
./cvt_test > cvt_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cvt_test
#
echo "Normal end of execution."
