#! /bin/bash
#
gfortran -c -Wall tsp_lau_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tsp_lau_test.o $HOME/lib/tsp_lau.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tsp_lau_test.o
#
mv a.out tsp_lau_test
./tsp_lau_test > tsp_lau_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm tsp_lau_test
#
echo "Normal end of execution."
