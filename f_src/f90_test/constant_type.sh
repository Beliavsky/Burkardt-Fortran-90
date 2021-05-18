#! /bin/bash
#
gfortran -c -Wall constant_type.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran constant_type.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm constant_type.o
#
mv a.out constant_type
./constant_type > constant_type.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm constant_type
#
echo "Normal end of execution."
