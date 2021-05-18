#! /bin/bash
#
gfortran -c -Wall ranlib.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ranlib.o ~/lib/ranlib.o
#
echo "Normal end of execution."
