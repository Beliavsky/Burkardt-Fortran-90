#! /bin/bash
#
gfortran -c -Wall lattice_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv lattice_rule.o ~/lib/lattice_rule.o
#
echo "Normal end of execution."
