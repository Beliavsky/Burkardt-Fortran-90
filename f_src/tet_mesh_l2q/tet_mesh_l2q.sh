#! /bin/bash
#
gfortran -c -Wall tet_mesh_l2q.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tet_mesh_l2q.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm tet_mesh_l2q.o
#
chmod ugo+x a.out
mv a.out ~/bin/tet_mesh_l2q
#
echo "Normal end of execution."
