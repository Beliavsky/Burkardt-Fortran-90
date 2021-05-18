#! /bin/bash
#
gfortran -c -Wall tsp_lau.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv tsp_lau.o ~/lib/tsp_lau.o
#
echo "Normal end of execution."
