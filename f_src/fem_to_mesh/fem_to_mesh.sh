#! /bin/bash
#
gfortran -c -Wall fem_to_mesh.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem_to_mesh.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm fem_to_mesh.o
#
chmod ugo+x a.out
mv a.out ~/bin/fem_to_mesh
#
echo "Normal end of execution."
