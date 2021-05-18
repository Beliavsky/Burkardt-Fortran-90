#! /bin/bash
#
gfortran -c -Wall hello.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hello.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hello.o
#
mv a.out hello
./hello > hello.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hello
#
echo "Normal end of execution."
