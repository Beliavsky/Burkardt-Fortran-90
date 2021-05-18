#! /bin/bash
#
gfortran -c -Wall image_edge.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv image_edge.o ~/lib/image_edge.o
#
echo "Normal end of execution."
