#! /bin/bash
#
gfortran -c -Wall stla_io.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv stla_io.o ~/lib/stla_io.o
#
echo "Normal end of execution."
