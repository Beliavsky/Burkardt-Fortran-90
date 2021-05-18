#! /bin/bash
#
gfortran -c -Wall cities_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cities_test cities_test.o $HOME/lib/cities.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cities_test.o
#
./cities_test > cities_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cities_test
#
echo "Normal end of execution."
