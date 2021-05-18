#! /bin/bash
#
gfortran -c -Wall tet_mesh_quality.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tet_mesh_quality.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tet_mesh_quality.o
#
chmod ugo+x a.out
mv a.out ~/bin/tet_mesh_quality
#
echo "Normal end of execution."
