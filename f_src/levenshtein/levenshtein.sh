#! /bin/bash
#
gfortran -c -Wall levenshtein.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv levenshtein.o ~/lib/levenshtein.o
#
echo "Normal end of execution."
