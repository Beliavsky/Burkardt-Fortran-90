#! /bin/bash
#
gfortran -c -Wall disk01_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv disk01_rule.o ~/lib/disk01_rule.o
#
echo "Normal end of execution."
