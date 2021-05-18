#! /bin/bash
#
gfortran -c -Wall special_functions_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran special_functions_test.o $HOME/lib/special_functions.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm special_functions_test.o
#
mv a.out special_functions_test
./special_functions_test > special_functions_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm special_functions_test
#
echo "Normal end of execution."
