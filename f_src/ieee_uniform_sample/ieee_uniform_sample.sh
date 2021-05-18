#! /bin/bash
#
gfortran -c -Wall ieee_uniform_sample.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ieee_uniform_sample.o ~/lib/ieee_uniform_sample.o
#
echo "Normal end of execution."
