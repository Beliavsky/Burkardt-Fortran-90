#! /bin/bash
#
gfortran -c -Wall triangulation_quality.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangulation_quality.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangulation_quality.o
#
chmod ugo+x a.out
mv a.out ~/bin/triangulation_quality
#
echo "Normal end of execution."
