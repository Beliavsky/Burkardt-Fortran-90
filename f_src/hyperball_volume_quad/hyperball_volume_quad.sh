#! /bin/bash
#
gfortran -c -Wall hyperball_volume_quad.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hyperball_volume_quad.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hyperball_volume_quad.o
#
mv a.out ~/bin/hyperball_volume_quad
#
echo "Normal end of execution."
