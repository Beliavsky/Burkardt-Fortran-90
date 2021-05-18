#! /bin/bash
#
gfortran -c -Wall svd_snowfall_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran svd_snowfall_test.o $HOME/lib/svd_snowfall.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm svd_snowfall_test.o
#
mv a.out svd_snowfall_test
./svd_snowfall_test > svd_snowfall_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm svd_snowfall_test
#
echo "Normal end of execution."
