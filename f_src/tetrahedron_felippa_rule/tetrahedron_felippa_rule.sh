#! /bin/bash
#
gfortran -c -Wall tetrahedron_felippa_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv tetrahedron_felippa_rule.o ~/lib/tetrahedron_felippa_rule.o
#
echo "Normal end of execution."
