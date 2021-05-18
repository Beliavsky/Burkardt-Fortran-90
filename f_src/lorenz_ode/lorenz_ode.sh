#! /bin/bash
#
gfortran -c -Wall lorenz_ode.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran lorenz_ode.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lorenz_ode.o
#
mv a.out ~/bin/lorenz_ode
#
echo "Normal end of execution."
