#! /bin/bash
#
gfortran -c -Wall triangle_felippa_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_felippa_rule.o ~/lib/triangle_felippa_rule.o
#
echo "Normal end of execution."
