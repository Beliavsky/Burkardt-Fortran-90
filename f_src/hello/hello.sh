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
mv a.out ~/bin/hello
#
echo "Normal end of execution."
