#! /bin/bash
#
gfortran -c -Wall disk_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv disk_rule.o ~/lib/disk_rule.o
#
echo "Normal end of execution."
