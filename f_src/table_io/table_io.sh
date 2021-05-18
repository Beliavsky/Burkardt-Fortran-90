#! /bin/bash
#
gfortran -c -Wall table_io.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv table_io.o ~/lib/table_io.o
#
echo "Normal end of execution."
