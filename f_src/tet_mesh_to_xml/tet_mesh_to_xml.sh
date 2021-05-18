#! /bin/bash
#
gfortran -c -Wall tet_mesh_to_xml.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tet_mesh_to_xml.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm tet_mesh_to_xml.o
#
chmod ugo+x a.out
mv a.out ~/bin/tet_mesh_to_xml
#
echo "Normal end of execution."
