#! /bin/bash
#
gfortran -c -Wall box_behnken.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv box_behnken.o ~/lib/box_behnken.o
#
echo "Normal end of execution."
