#! /bin/bash
#
gfortran -c -Wall latinize.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv latinize.o ~/lib/latinize.o
#
echo "Normal end of execution."
