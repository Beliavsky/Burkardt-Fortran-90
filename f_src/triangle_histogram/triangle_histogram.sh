#! /bin/bash
#
gfortran -c -Wall triangle_histogram.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangle_histogram.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle_histogram.o
#
chmod ugo+x a.out
mv a.out ~/bin/triangle_histogram
#
echo "Normal end of execution."
