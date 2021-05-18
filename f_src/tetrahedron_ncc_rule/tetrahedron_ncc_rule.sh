#! /bin/bash
#
gfortran -c -Wall tetrahedron_ncc_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv tetrahedron_ncc_rule.o ~/lib/tetrahedron_ncc_rule.o
#
echo "Normal end of execution."
