#! /bin/bash
#
gfortran -c -Wall sphere_design_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o sphere_design_rule_test sphere_design_rule_test.o $HOME/lib/sphere_design_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_design_rule_test.o
#
./sphere_design_rule_test > sphere_design_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sphere_design_rule_test
#
echo "Normal end of execution."
