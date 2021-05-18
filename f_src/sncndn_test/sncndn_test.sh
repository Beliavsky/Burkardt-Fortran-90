#! /bin/bash
#
gfortran -c -Wall sncndn_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o sncndn_test sncndn_test.o $HOME/lib/sncndn.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sncndn_test.o
#
./sncndn_test > sncndn_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sncndn_test
#
echo "Normal end of execution."
