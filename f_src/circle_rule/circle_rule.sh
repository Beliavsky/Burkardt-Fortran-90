#! /bin/bash
#
gfortran -c -Wall circle_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv circle_rule.o ~/lib/circle_rule.o
#
echo "Normal end of execution."
