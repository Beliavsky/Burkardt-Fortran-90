#! /bin/bash
#
gfortran -c -Wall truncated_normal.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv truncated_normal.o ~/lib/truncated_normal.o
#
echo "Normal end of execution."
