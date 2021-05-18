#! /bin/bash
#
gfortran -c -Wall truncated_normal_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran truncated_normal_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm truncated_normal_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/truncated_normal_rule
#
echo "Normal end of execution."
