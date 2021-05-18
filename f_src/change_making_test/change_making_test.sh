#! /bin/bash
#
gfortran -c -Wall change_making_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o change_making_test change_making_test.o $HOME/lib/change_making.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm change_making_test.o
#
./change_making_test > change_making_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm change_making_test
#
echo "Normal end of execution."
