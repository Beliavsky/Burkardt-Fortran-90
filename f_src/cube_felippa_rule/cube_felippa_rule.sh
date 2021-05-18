#! /bin/bash
#
gfortran -c -Wall cube_felippa_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cube_felippa_rule.o ~/lib/cube_felippa_rule.o
#
echo "Normal end of execution."
