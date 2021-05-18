#! /bin/bash
#
gfortran -c -Wall local_min_rc.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv local_min_rc.o ~/lib/local_min_rc.o
#
echo "Normal end of execution."
