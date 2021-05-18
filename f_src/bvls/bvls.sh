#! /bin/bash
#
gfortran -c -Wall bvls.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv bvls.o ~/lib/bvls.o
#
echo "Normal end of execution."
