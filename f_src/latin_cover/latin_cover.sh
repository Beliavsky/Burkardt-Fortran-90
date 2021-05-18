#! /bin/bash
#
gfortran -c -Wall latin_cover.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv latin_cover.o ~/lib/latin_cover.o
#
echo "Normal end of execution."
