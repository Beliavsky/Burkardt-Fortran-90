#! /bin/bash
#
gfortran -c -Wall test_triangulation.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_triangulation.o ~/lib/test_triangulation.o
#
echo "Normal end of execution."
