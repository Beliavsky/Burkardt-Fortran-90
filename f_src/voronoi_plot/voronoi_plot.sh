#! /bin/bash
#
gfortran -c -Wall voronoi_plot.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran voronoi_plot.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm voronoi_plot.o
#
chmod ugo+x a.out
mv a.out ~/bin/voronoi_plot
#
echo "Normal end of execution."
