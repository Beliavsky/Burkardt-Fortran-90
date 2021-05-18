#! /bin/bash
#
gfortran -c -Wall square_felippa_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran square_felippa_rule_test.o $HOME/lib/square_felippa_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm square_felippa_rule_test.o
#
mv a.out square_felippa_rule_test
./square_felippa_rule_test > square_felippa_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Errors running square_felippa_rule_test"
  exit
fi
rm square_felippa_rule_test
#
echo "Normal end of execution."
