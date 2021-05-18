#! /bin/bash
#
gfortran -c -Wall beta_nc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o beta_nc_test beta_nc_test.o $HOME/lib/beta_nc.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm beta_nc_test.o
#
./beta_nc_test > beta_nc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm beta_nc_test
#
echo "Normal end of execution."
