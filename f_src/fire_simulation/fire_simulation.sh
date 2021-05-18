#! /bin/bash
#
gfortran -c -Wall fire_simulation.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fire_simulation.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fire_simulation.o
#
mv a.out $HOME/bin/fire_simulation
#
echo "Normal end of execution."
