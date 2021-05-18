#! /bin/bash
#
gfortran -c -Wall square_felippa_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv square_felippa_rule.o ~/lib/square_felippa_rule.o
#
echo "Normal end of execution."
