#! /bin/bash
#
gfortran -c -Wall hankel_spd_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o hankel_spd_test hankel_spd_test.o $HOME/lib/hankel_spd.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hankel_spd_test.o
#
./hankel_spd_test > hankel_spd_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hankel_spd_test
#
echo "Normal end of execution."
