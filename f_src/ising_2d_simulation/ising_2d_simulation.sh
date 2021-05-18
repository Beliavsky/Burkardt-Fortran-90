#! /bin/bash
#
gfortran -c -Wall ising_2d_simulation.f90
if [ $? -ne 0 ]; then
    echo "Compile error."
  exit
fi
#
gfortran ising_2d_simulation.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ising_2d_simulation.o
#
chmod ugo+x a.out
mv a.out ~/bin/ising_2d_simulation
#
echo "Normal end of execution."
