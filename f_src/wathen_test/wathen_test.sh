#! /bin/bash
#
gfortran -c -Wall wathen_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran wathen_test.o /$HOME/lib/wathen.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm wathen_test.o
#
mv a.out wathen_test
./wathen_test > wathen_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm wathen_test
#
echo "Normal end of execution."
