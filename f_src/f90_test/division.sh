#! /bin/bash
#
gfortran -c -Wall division.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran division.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm division.o
#
mv a.out division
./division > division.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm division
#
echo "Normal end of execution."
