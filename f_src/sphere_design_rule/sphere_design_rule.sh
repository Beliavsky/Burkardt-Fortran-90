#! /bin/bash
#
gfortran -c -Wall sphere_design_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sphere_design_rule.o ~/lib/sphere_design_rule.o
#
echo "Normal end of execution."
