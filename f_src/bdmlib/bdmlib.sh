#! /bin/bash
#
gfortran -c -Wall bdmlib.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv bdmlib.o ~/lib/bdmlib.o
#
echo "Normal end of execution."
