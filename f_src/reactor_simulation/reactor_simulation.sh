#! /bin/bash
#
gfortran -c -Wall reactor_simulation.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran reactor_simulation.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm reactor_simulation.o
#
mv a.out $HOME/bin/reactor_simulation
#
echo "Normal end of execution."
