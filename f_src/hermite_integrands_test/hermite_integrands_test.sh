#! /bin/bash
#
gfortran -c -Wall hermite_integrands_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hermite_integrands_test.o $HOME/lib/hermite_integrands.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hermite_integrands_test.o
#
mv a.out hermite_integrands_test
./hermite_integrands_test > hermite_integrands_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hermite_integrands_test
#
echo "Normal end of execution."
