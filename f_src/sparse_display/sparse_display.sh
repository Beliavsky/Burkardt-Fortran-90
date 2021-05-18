#! /bin/bash
#
gfortran -c -Wall sparse_display.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sparse_display.o ~/lib/sparse_display.o
#
echo "Normal end of execution."
