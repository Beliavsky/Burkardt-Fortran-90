#! /bin/bash
#
gfortran -c -Wall sde.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sde.o ~/lib/sde.o
#
echo "Normal end of execution."
