#! /bin/bash
#
gfortran -c -Wall sncndn.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sncndn.o ~/lib/sncndn.o
#
echo "Normal end of execution."
