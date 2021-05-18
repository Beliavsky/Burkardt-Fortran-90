#! /bin/bash
#
gfortran -c -Wall tetrahedron_keast_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv tetrahedron_keast_rule.o ~/lib/tetrahedron_keast_rule.o
#
echo "Normal end of execution."
