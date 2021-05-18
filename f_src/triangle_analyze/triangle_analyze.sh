#! /bin/bash
#
gfortran -c -Wall triangle_analyze.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangle_analyze.o $HOME/lib/triangle_properties.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle_analyze.o
#
chmod ugo+x a.out
mv a.out ~/bin/triangle_analyze
#
echo "Normal end of execution."
