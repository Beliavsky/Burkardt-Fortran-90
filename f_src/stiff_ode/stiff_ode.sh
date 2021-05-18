#! /bin/bash
#
gfortran -c -Wall stiff_ode.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv stiff_ode.o ~/lib/stiff_ode.o
#
echo "Normal end of execution."
