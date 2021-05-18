#! /bin/bash
#
gfortran -c -Wall polyominoes.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv polyominoes.o ~/lib/polyominoes.o
#
echo "Normal end of execution."
