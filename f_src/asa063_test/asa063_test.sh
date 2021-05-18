#! /bin/bash
#
gfortran -c -Wall asa063_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran asa063_test.o $HOME/lib/asa063.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa063_test.o
#
mv a.out asa063_test
./asa063_test > asa063_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa063_test
#
echo "Normal end of execution."
