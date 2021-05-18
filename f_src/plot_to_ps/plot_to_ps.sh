#! /bin/bash
#
gfortran -c -Wall plot_to_ps.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran plot_to_ps.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm plot_to_ps.o
#
chmod ugo+x a.out
mv a.out ~/bin/plot_to_ps
#
echo "Normal end of execution."
