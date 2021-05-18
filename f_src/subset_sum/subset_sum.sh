#! /bin/bash
#
gfortran -c -Wall subset_sum.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv subset_sum.o ~/lib/subset_sum.o
#
echo "Normal end of execution."
