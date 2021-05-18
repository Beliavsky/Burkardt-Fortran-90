#! /bin/bash
#
gfortran -c -Wall fd1d_predator_prey.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fd1d_predator_prey.o ~/lib/fd1d_predator_prey.o
#
echo "Normal end of execution."
