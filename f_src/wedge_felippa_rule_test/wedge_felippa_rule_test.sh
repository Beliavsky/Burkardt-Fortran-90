#! /bin/bash
#
gfortran -c -Wall wedge_felippa_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran wedge_felippa_rule_test.o $HOME/lib/wedge_felippa_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm wedge_felippa_rule_test.o
#
mv a.out wedge_felippa_rule_test
./wedge_felippa_rule_test > wedge_felippa_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm wedge_felippa_rule_test
#
echo "Normal end of execution."
