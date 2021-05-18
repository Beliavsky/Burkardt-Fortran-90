#! /bin/bash
#
gfortran -c -Wall image_components.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv image_components.o ~/lib/image_components.o
#
echo "Normal end of execution."
