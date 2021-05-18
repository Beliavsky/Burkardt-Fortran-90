#! /bin/bash
#
gfortran -o f90_stop_test1 -Wall f90_stop_test1.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
./f90_stop_test1 &> f90_stop_test.txt
if [ $? -ne 0 ]; then
  echo "Run time error in f90_stop_test1"
fi
rm f90_stop_test1
#
gfortran -o f90_stop_test2 -Wall f90_stop_test2.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
./f90_stop_test2 &>> f90_stop_test.txt
if [ $? -ne 0 ]; then
  echo "Run time error in f90_stop_test2"
fi
rm f90_stop_test2
#
echo "Normal end of execution."
