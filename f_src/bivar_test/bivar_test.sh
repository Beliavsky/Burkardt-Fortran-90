#! /bin/bash
#
gfortran -c -Wall bivar_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o bivar_test bivar_test.o $HOME/lib/bivar.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bivar_test.o
#
./bivar_test > bivar_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bivar_test
#
echo "Normal end of execution."
