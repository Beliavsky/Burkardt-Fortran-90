#! /bin/bash
#
gfortran -c -Wall triangle_wandzura_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_wandzura_rule.o ~/lib/triangle_wandzura_rule.o
#
echo "Normal end of execution."
