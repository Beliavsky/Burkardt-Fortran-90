#! /bin/bash
#
gfortran -c -Wall blend_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o blend_test blend_test.o $HOME/lib/blend.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm blend_test.o
#
./blend_test > blend_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm blend_test
#
echo "Normal end of execution."
