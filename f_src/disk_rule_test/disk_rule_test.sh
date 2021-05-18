#! /bin/bash
#
gfortran -c -Wall disk_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o disk_rule_test disk_rule_test.o $HOME/lib/disk_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm disk_rule_test.o
#
./disk_rule_test > disk_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm disk_rule_test
#
echo "Normal end of execution."
