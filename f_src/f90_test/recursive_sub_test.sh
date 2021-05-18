#! /bin/bash
#
gfortran -c -Wall recursive_sub_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran recursive_sub_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm recursive_sub_test.o
#
mv a.out recursive_sub_test
./recursive_sub_test > recursive_sub_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm recursive_sub_test
#
echo "Normal end of execution."
