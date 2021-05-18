#! /bin/bash
#
gfortran -c -Wall sobol_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sobol_test.o $HOME/lib/sobol.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sobol_test.o
#
mv a.out sobol_test
./sobol_test > sobol_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sobol_test
#
echo "Normal end of execution."
