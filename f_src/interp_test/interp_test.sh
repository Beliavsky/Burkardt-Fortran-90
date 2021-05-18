#! /bin/bash
#
gfortran -c -Wall interp_test.f90
if [ $? -ne 0 ]; then
    echo "Compile error."
  exit
fi
#
gfortran interp_test.o $HOME/lib/interp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm interp_test.o
#
mv a.out interp_test
./interp_test > interp_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm interp_test
#
echo "Normal end of execution."
