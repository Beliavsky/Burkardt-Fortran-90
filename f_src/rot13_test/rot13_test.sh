#! /bin/bash
#
gfortran -c -Wall rot13_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o rot13_test rot13_test.o $HOME/lib/rot13.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm rot13_test.o
#
./rot13_test > rot13_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm rot13_test
#
echo "Normal end of execution."
