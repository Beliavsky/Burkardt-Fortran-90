#! /bin/bash
#
gfortran -c -Wall padua.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv padua.o ~/lib/padua.o
#
echo "Normal end of execution."
