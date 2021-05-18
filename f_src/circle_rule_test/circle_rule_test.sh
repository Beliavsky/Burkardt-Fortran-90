#! /bin/bash
#
gfortran -c -Wall circle_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o circle_rule_test circle_rule_test.o $HOME/lib/circle_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm circle_rule_test.o
#
./circle_rule_test > circle_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm circle_rule_test
#
echo "Normal end of execution."
