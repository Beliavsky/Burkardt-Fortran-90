#! /bin/bash
#
gfortran -c -Wall ziggurat_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ziggurat_test.o $HOME/lib/ziggurat.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ziggurat_test.o
#
mv a.out ziggurat_test
./ziggurat_test > ziggurat_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ziggurat_test
#
echo "Normal end of execution."
