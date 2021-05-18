#! /bin/bash
#
gfortran -c -Wall codepack_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o codepack_test codepack_test.o $HOME/lib/codepack.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm codepack_test.o
#
./codepack_test > codepack_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm codepack_test
#
echo "Normal end of execution."
