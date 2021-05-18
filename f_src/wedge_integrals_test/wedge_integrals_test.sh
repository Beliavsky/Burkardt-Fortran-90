#! /bin/bash
#
gfortran -c -Wall wedge_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran wedge_integrals_test.o $HOME/lib/wedge_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm wedge_integrals_test.o
#
mv a.out wedge_integrals_test
./wedge_integrals_test > wedge_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm wedge_integrals_test
#
echo "Normal end of execution."
