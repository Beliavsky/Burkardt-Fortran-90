#! /bin/bash
#
gfortran -c -Wall triangle_dunavant_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_dunavant_rule.o ~/lib/triangle_dunavant_rule.o
#
echo "Normal end of execution."
