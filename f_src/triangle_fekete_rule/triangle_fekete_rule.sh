#! /bin/bash
#
gfortran -c -Wall triangle_fekete_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_fekete_rule.o ~/lib/triangle_fekete_rule.o
#
echo "Normal end of execution."
