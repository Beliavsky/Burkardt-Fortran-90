#! /bin/bash
#
gfortran -c -Wall fd_predator_prey.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fd_predator_prey.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fd_predator_prey.o
#
mv a.out ~/bin/fd_predator_prey
#
echo "Normal end of execution."
