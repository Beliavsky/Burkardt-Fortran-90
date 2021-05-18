#! /bin/bash
#
gfortran -c -Wall errors.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv errors.o ~/lib
#
echo "Normal end of execution."
