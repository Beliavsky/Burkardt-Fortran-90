#! /bin/bash
#
gfortran -c -Wall halton_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o halton_test halton_test.o $HOME/lib/halton.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm halton_test.o
#
./halton_test > halton_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm halton_test
#
echo "Normal end of execution."
