#! /bin/bash
#
gfortran -c -Wall test_min.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_min.o ~/lib
#
echo "Normal end of execution."

