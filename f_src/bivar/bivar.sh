#! /bin/bash
#
gfortran -c -Wall bivar.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv bivar.o ~/lib/bivar.o
#
echo "Normal end of execution."
