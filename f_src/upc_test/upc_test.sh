#! /bin/bash
#
gfortran -c -Wall upc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran upc_test.o $HOME/lib/upc.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm upc_test.o
#
mv a.out upc_test
./upc_test > upc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm upc_test
#
echo "Normal end of execution."
