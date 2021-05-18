#! /bin/bash
#
gfortran -c -Wall power_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran power_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm power_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/power_rule
#
echo "Normal end of execution."
