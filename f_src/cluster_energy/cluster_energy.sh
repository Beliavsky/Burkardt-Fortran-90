#! /bin/bash
#
gfortran -c -Wall cluster_energy.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran cluster_energy.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cluster_energy.o
#
chmod ugo+x a.out
mv a.out ~/bin/cluster_energy
#
echo "Normal end of execution."
