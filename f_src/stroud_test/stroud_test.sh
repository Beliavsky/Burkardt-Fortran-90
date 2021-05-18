#! /bin/bash
#
gfortran -c -Wall stroud_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran stroud_test.o $HOME/lib/stroud.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm stroud_test.o
#
mv a.out stroud_test
./stroud_test > stroud_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm stroud_test
#
echo "Normal end of execution."
