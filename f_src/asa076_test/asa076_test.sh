#! /bin/bash
#
gfortran -c -Wall asa076_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa076_test asa076_test.o /$HOME/lib/asa076.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa076_test.o
#
./asa076_test > asa076_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa076_test
#
echo "Normal end of execution."
