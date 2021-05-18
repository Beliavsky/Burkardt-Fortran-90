#! /bin/bash
#
gfortran -c -Wall kmeans.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv kmeans.o ~/lib/kmeans.o
#
echo "Normal end of execution."
