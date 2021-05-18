#! /bin/bash
#
gfortran -c -Wall annulus_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o annulus_rule_test annulus_rule_test.o $HOME/lib/annulus_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
./annulus_rule_test > annulus_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm annulus_rule_test
#
echo "Normal end of execution."
