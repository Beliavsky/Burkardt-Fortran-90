#! /bin/bash
#
gfortran -c -Wall walsh.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv walsh.o ~/lib/walsh.o
#
echo "Normal end of execution."
