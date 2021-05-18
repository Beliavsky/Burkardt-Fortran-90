#! /bin/bash
#
gfortran -c -Wall hypercube_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hypercube_integrals_test.o $HOME/lib/hypercube_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hypercube_integrals_test.o
#
mv a.out hypercube_integrals_test
./hypercube_integrals_test > hypercube_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hypercube_integrals_test
#
echo "Normal end of execution."
