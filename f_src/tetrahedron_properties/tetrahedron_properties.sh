#! /bin/bash
#
gfortran -c -Wall tetrahedron_properties.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tetrahedron_properties.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tetrahedron_properties.o
#
chmod ugo+x a.out
mv a.out ~/bin/tetrahedron_properties
#
echo "Normal end of execution."
