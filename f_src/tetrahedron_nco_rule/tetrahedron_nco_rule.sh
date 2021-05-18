#! /bin/bash
#
gfortran -c -Wall tetrahedron_nco_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv tetrahedron_nco_rule.o ~/lib/tetrahedron_nco_rule.o
#
echo "Normal end of execution."
