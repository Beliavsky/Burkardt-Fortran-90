#! /bin/bash
#
gfortran -c -Wall random_data_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran random_data_test.o $HOME/lib/random_data.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm random_data_test.o
#
mv a.out random_data_test
./random_data_test > random_data_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm random_data_test
#
echo "Normal end of execution."
