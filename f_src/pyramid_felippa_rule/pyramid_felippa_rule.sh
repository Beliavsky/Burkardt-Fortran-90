#! /bin/bash
#
gfortran -c -Wall pyramid_felippa_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv pyramid_felippa_rule.o ~/lib/pyramid_felippa_rule.o
#
echo "Normal end of execution."
