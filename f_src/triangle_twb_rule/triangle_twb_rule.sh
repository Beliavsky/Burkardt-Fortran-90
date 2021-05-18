#! /bin/bash
#
gfortran -c -Wall triangle_twb_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_twb_rule.o ~/lib/triangle_twb_rule.o
#
echo "Normal end of execution."
