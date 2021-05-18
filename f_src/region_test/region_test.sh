#! /bin/bash
#
gfortran -c -Wall region_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran region_test.o $HOME/lib/region.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm region_test.o
#
mv a.out region_test
./region_test > region_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm region_test
#
echo "Normal end of execution."
