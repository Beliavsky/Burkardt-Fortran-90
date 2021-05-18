#! /bin/bash
#
gfortran -c -Wall laguerre_integrands_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran laguerre_integrands_test.o $HOME/lib/laguerre_integrands.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm laguerre_integrands_test.o
#
mv a.out laguerre_integrands_test
./laguerre_integrands_test > laguerre_integrands_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm laguerre_integrands_test
#
echo "Normal end of execution."
