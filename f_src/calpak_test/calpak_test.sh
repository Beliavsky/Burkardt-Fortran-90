#! /bin/bash
#
gfortran -c -Wall calpak_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o calpak_test calpak_test.o $HOME/lib/calpak.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm calpak_test.o
#
./calpak_test > calpak_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm calpak_test
#
echo "Normal end of execution."
