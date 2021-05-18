#! /bin/bash
#
gfortran -c -Wall filum_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran filum_test.o $HOME/lib/filum.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm filum_test.o
#
mv a.out filum_test
./filum_test > filum_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm filum_test
#
echo "Normal end of execution."
