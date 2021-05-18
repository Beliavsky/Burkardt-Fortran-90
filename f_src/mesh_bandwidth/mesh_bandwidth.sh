#! /bin/bash
#
gfortran -c -Wall mesh_bandwidth.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran mesh_bandwidth.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mesh_bandwidth.o
#
chmod ugo+x a.out
mv a.out ~/bin/mesh_bandwidth
#
echo "Normal end of execution."
