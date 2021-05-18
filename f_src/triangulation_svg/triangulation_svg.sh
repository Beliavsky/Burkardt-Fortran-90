#! /bin/bash
#
gfortran -c -Wall triangulation_svg.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangulation_svg.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm triangulation_svg.o
#
chmod ugo+x a.out
mv a.out ~/bin/triangulation_svg
#
echo "Normal end of execution."
