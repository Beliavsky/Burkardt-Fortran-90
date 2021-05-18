#! /bin/bash
#
gfortran -c -Wall tetrahedron_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv tetrahedron_integrals.o ~/lib/tetrahedron_integrals.o
#
echo "Normal end of execution."
