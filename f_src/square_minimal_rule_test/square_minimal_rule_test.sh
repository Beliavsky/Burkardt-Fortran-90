#! /bin/bash
#
gfortran -c -Wall square_minimal_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o square_minimal_rule_test square_minimal_rule_test.o \
  $HOME/lib/square_minimal_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm square_minimal_rule_test.o
#
./square_minimal_rule_test > square_minimal_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm square_minimal_rule_test
#
echo "Normal end of execution."
