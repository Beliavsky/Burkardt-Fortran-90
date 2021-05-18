#! /bin/bash
#
gfortran -c -Wall hypersphere_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hypersphere_integrals_test.o $HOME/lib/hypersphere_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hypersphere_integrals_test.o
#
mv a.out hypersphere_integrals_test
./hypersphere_integrals_test > hypersphere_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hypersphere_integrals_test
#
echo "Normal end of execution."
