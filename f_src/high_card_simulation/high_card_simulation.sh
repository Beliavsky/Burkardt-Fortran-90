#! /bin/bash
#
gfortran -c -Wall high_card_simulation.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv high_card_simulation.o ~/lib/high_card_simulation.o
#
echo "Normal end of execution."
