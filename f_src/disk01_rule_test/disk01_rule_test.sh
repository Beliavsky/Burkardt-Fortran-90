#! /bin/bash
#
gfortran -c -Wall disk01_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o disk01_rule_test disk01_rule_test.o $HOME/lib/disk01_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm disk01_rule_test.o
#
./disk01_rule_test > disk01_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm disk01_rule_test
#
echo "Normal end of execution."
