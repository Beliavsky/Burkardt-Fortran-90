#! /bin/bash
#
gfortran -c -Wall f90_random_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran f90_random_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm f90_random_test.o
#
mv a.out f90_random_test
./f90_random_test > f90_random_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm f90_random_test
#
echo "Normal end of execution."
