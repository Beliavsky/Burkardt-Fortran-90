#! /bin/bash
#
gfortran -c -Wall triangulation_quad.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangulation_quad.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangulation_quad.o
#
chmod ugo+x a.out
mv a.out ~/bin/triangulation_quad
#
echo "Normal end of execution."
