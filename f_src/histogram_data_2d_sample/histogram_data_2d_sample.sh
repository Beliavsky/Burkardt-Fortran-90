#! /bin/bash
#
gfortran -c -Wall histogram_data_2d_sample.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran histogram_data_2d_sample.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm histogram_data_2d_sample.o
#
mv a.out ~/bin/histogram_data_2d_sample
#
echo "Normal end of execution."
