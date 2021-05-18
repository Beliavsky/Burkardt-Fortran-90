#! /bin/bash
#
gfortran -c -Wall triangulation_plot.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangulation_plot.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm triangulation_plot.o
#
chmod ugo+x a.out
mv a.out ~/bin/triangulation_plot
#
echo "Normal end of execution."
