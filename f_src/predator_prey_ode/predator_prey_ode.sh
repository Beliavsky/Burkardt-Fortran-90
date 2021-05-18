#! /bin/bash
#
gfortran -c -Wall predator_prey_ode.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv predator_prey_ode.o ~/lib/predator_prey_ode.o
#
echo "Normal end of execution."
