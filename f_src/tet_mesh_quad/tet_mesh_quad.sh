#! /bin/bash
#
gfortran -c -Wall tet_mesh_quad.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tet_mesh_quad.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tet_mesh_quad.o
#
chmod ugo+x a.out
mv a.out ~/bin/tet_mesh_quad
#
echo "Normal end of execution."
