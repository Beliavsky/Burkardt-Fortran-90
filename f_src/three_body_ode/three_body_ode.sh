#! /bin/bash
#
gfortran -c -Wall three_body_ode.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv three_body_ode.o ~/lib/three_body_ode.o
#
echo "Normal end of execution."
