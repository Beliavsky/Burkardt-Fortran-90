#! /bin/bash
#
gfortran -c -Wall st_io.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv st_io.o ~/lib/st_io.o
#
echo "Normal end of execution."
