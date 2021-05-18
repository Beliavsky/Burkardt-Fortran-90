#! /bin/bash
#
gfortran -c -Wall csv_io.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv csv_io.o ~/lib/csv_io.o
#
echo "Normal end of execution."
