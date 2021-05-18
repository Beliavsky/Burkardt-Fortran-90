#! /bin/bash
#
gfortran -c -Wall atbash_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o atbash_test atbash_test.o $HOME/lib/atbash.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm atbash_test.o
#
./atbash_test > atbash_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm atbash_test
#
echo "Normal end of execution."
