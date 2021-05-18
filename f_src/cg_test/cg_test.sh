#! /bin/bash
#
gfortran -c -Wall cg_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cg_test cg_test.o $HOME/lib/cg.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cg_test.o
#
./cg_test > cg_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cg_test
#
echo "Normal end of execution."
