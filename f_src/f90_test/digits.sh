#! /bin/bash
#
gfortran -c -Wall digits.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran digits.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm digits.o
#
mv a.out digits
./digits > digits.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm digits
#
echo "Normal end of execution."
