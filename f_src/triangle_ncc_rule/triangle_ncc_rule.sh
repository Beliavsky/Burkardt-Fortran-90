#! /bin/bash
#
gfortran -c -Wall triangle_ncc_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_ncc_rule.o ~/lib/triangle_ncc_rule.o
#
echo "Normal end of execution."
