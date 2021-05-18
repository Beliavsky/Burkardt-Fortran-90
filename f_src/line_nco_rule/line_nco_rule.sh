#! /bin/bash
#
gfortran -c -Wall line_nco_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv line_nco_rule.o ~/lib/line_nco_rule.o
#
echo "Normal end of execution."
