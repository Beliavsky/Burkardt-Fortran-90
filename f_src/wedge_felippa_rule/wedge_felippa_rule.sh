#! /bin/bash
#
gfortran -c -Wall wedge_felippa_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv wedge_felippa_rule.o ~/lib/wedge_felippa_rule.o
#
echo "Normal end of execution."
