#! /bin/bash
#
gfortran -c -Wall divdif_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran divdif_test.o /$HOME/lib/divdif.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm divdif_test.o
#
mv a.out divdif_test
./divdif_test > divdif_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm divdif_test
#
echo "Normal end of execution."
