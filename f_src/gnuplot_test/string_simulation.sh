#! /bin/bash
#
gfortran -c -Wall string_simulation.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran string_simulation.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm string_simulation.o
mv a.out ~/bin/string_simulation
#
echo "Normal end of execution."
