#! /bin/bash
#
gfortran -c -Wall codepack.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv codepack.o ~/lib/codepack.o
#
echo "Normal end of execution."
