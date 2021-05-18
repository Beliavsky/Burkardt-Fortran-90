#! /bin/bash
#
gfortran -c -Wall normal_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran normal_test.o $HOME/lib/normal.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm normal_test.o
#
mv a.out normal_test
./normal_test > normal_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm normal_test
#
echo "Normal end of execution."
