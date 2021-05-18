#! /bin/bash
#
gfortran -c -Wall bvec.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv bvec.o ~/lib/bvec.o
#
echo "Normal end of execution."
