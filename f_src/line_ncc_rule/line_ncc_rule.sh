#! /bin/bash
#
gfortran -c -Wall line_ncc_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv line_ncc_rule.o ~/lib/line_ncc_rule.o
#
echo "Normal end of execution."
