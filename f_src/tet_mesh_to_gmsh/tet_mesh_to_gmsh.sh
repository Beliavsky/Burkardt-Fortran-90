#! /bin/bash
#
gfortran -c -Wall tet_mesh_to_gmsh.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tet_mesh_to_gmsh.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm tet_mesh_to_gmsh.o
#
chmod ugo+x a.out
mv a.out ~/bin/tet_mesh_to_gmsh
#
echo "Normal end of execution."
