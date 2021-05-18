#! /bin/bash
#
gfortran -c -Wall minpack.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv minpack.o ~/lib/minpack.o
#
echo "Normal end of execution."
