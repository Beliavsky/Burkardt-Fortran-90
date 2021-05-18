#! /bin/bash
#
gfortran -c -Wall fd2d_predator_prey.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fd2d_predator_prey.o ~/lib
#
echo "Normal end of execution."
