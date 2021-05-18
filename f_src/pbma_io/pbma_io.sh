#! /bin/bash
#
gfortran -c -Wall pbma_io.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv pbma_io.o ~/lib/pbma_io.o
#
echo "Normal end of execution."
