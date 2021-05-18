#! /bin/bash
#
gfortran -c -Wall alpert_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv alpert_rule.o ~/lib/alpert_rule.o
#
echo "Normal end of execution."
