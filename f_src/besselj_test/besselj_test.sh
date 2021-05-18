#! /bin/bash
#
gfortran -c -Wall besselj_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o besselj_test besselj_test.o $HOME/lib/besselj.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm besselj_test.o
#
./besselj_test > besselj_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm besselj_test
#
echo "Normal end of execution."
