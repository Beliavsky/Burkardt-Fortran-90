#! /bin/bash
#
gfortran -c -Wall pppack_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o pppack_test pppack_test.o /$HOME/lib/pppack.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pppack_test.o
#
./pppack_test > pppack_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pppack_test
#
echo "Normal end of execution."
