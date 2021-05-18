#! /bin/bash
#
gfortran -c -Wall ziggurat.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ziggurat.o ~/lib/ziggurat.o
#
echo "Normal end of execution."
