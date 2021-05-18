#! /bin/bash
#
gfortran -c -Wall asa082_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa082_test asa082_test.o /$HOME/lib/asa082.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa082_test.o
#
./asa082_test > asa082_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa082_test
#
echo "Normal end of execution."
