#! /bin/bash
#
gfortran -c -Wall ode.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ode.o ~/lib/ode.o
#
echo "Normal end of execution."
