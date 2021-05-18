#! /bin/bash
#
gfortran -c -Wall recursive_fun_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran recursive_fun_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm recursive_fun_test.o
#
mv a.out recursive_fun_test
./recursive_fun_test > recursive_fun_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm recursive_fun_test
#
echo "Normal end of execution."
