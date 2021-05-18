#! /bin/bash
#
gfortran -c -Wall cube_arbq_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cube_arbq_rule.o ~/lib/cube_arbq_rule.o
#
echo "Normal end of execution."
