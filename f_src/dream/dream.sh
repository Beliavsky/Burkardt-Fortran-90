#! /bin/bash
#
gfortran -c -Wall dream.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv dream.o ~/lib/dream.o
#
echo "Normal end of execution."
