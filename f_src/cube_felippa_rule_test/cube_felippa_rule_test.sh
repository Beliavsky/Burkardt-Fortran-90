#! /bin/bash
#
gfortran -c -Wall cube_felippa_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cube_felippa_rule_test cube_felippa_rule_test.o $HOME/lib/cube_felippa_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cube_felippa_rule_test.o
#
./cube_felippa_rule_test > cube_felippa_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cube_felippa_rule_test
#
echo "Normal end of execution."
