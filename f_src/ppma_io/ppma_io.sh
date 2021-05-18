#! /bin/bash
#
gfortran -c -Wall ppma_io.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ppma_io.o ~/lib/ppma_io.o
#
echo "Normal end of execution."
