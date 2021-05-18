#! /bin/bash
#
gfortran -c -Wall triangle01_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangle01_integrals_test.o $HOME/lib/triangle01_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle01_integrals_test.o
#
mv a.out triangle01_integrals_test
./triangle01_integrals_test > triangle01_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm triangle01_integrals_test
#
echo "Normal end of execution."
