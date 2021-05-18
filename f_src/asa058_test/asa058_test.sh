#! /bin/bash
#
gfortran -c -Wall asa058_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran asa058_test.o $HOME/lib/asa058.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa058_test.o
#
mv a.out asa058_test
./asa058_test > asa058_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa058_test
#
echo "Normal end of execution."
