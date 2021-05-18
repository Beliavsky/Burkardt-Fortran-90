#! /bin/bash
#
gfortran -c -Wall hammersley.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hammersley.o ~/lib/hammersley.o
#
echo "Normal end of execution."
