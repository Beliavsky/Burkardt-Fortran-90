#! /bin/bash
#
gfortran -c -Wall cordic_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cordic_test cordic_test.o $HOME/lib/cordic.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cordic_test.o
#
./cordic_test > cordic_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cordic_test
#
echo "Normal end of execution."
