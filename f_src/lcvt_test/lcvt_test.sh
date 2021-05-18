#! /bin/bash
#
gfortran -c -Wall lcvt_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran lcvt_test.o $HOME/lib/lcvt.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lcvt_test.o
#
mv a.out lcvt_test
./lcvt_test > lcvt_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm lcvt_test
#
echo "Normal end of execution."
