#! /bin/bash
#
gfortran -c -Wall quality_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran quality_test.o $HOME/lib/quality.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quality_test.o
#
mv a.out quality_test
./quality_test > quality_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm quality_test
#
echo "Normal end of execution."
