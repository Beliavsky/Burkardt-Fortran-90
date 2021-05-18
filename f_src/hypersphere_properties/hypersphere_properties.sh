#! /bin/bash
#
gfortran -c -Wall hypersphere_properties.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hypersphere_properties.o ~/lib/hypersphere_properties.o
#
echo "Normal end of execution."
