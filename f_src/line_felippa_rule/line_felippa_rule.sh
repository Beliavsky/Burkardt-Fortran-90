#! /bin/bash
#
gfortran -c -Wall line_felippa_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv line_felippa_rule.o ~/lib/line_felippa_rule.o
#
echo "Normal end of execution."
