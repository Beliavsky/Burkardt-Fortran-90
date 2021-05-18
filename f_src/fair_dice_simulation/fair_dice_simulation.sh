#! /bin/bash
#
gfortran -c -Wall fair_dice_simulation.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fair_dice_simulation.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fair_dice_simulation.o
#
chmod ugo+x a.out
mv a.out ~/bin/fair_dice_simulation
#
echo "Normal end of execution."
