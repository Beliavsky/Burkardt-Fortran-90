#! /bin/bash
#
gfortran -c -Wall nintlib_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran nintlib_test.o $HOME/lib/nintlib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm nintlib_test.o
#
mv a.out nintlib_test
./nintlib_test > nintlib_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm nintlib_test
#
echo "Normal end of execution."
