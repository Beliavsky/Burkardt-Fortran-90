#! /bin/bash
#
gfortran -c -Wall specfun_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran specfun_test.o $HOME/lib/specfun.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm specfun_test.o
#
mv a.out specfun_test
./specfun_test > specfun_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm specfun_test
#
echo "Normal end of execution."
