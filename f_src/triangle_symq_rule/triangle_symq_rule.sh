#! /bin/bash
#
gfortran -c -Wall triangle_symq_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_symq_rule.o ~/lib/triangle_symq_rule.o
#
echo "Normal end of execution."
