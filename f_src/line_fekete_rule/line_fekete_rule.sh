#! /bin/bash
#
gfortran -c -Wall line_fekete_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv line_fekete_rule.o ~/lib/line_fekete_rule.o
#
echo "Normal end of execution."
