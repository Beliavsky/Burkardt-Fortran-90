#! /bin/bash
#
gfortran -c -Wall triangle_nco_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_nco_rule.o ~/lib/triangle_nco_rule.o
#
echo "Normal end of execution."
