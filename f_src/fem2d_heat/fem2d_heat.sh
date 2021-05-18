#! /bin/bash
#
gfortran -c -Wall fem2d_heat.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem2d_heat.o ~/lib
#
echo "Normal end of execution."
