#! /bin/bash
#
gfortran -c -Wall hammersley_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o hammersley_test hammersley_test.o $HOME/lib/hammersley.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hammersley_test.o
#
./hammersley_test > hammersley_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hammersley_test
#
echo "Normal end of execution."
