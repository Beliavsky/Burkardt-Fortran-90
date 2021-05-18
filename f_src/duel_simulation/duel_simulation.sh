#! /bin/bash
#
gfortran -c -Wall duel_simulation.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran duel_simulation.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm duel_simulation.o
#
chmod ugo+x a.out
mv a.out ~/bin/duel_simulation
#
echo "Normal end of execution."
