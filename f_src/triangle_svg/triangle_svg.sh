#! /bin/bash
#
gfortran -c -Wall triangle_svg.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_svg.o ~/lib/triangle_svg.o
#
echo "Normal end of execution."
