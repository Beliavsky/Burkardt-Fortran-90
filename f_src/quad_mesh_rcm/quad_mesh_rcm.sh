#! /bin/bash
#
gfortran -c -Wall quad_mesh_rcm.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran quad_mesh_rcm.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quad_mesh_rcm.o
#
chmod ugo+x a.out
mv a.out ~/bin/quad_mesh_rcm
#
echo "Normal end of execution."
