#! /bin/bash
#
gfortran -c -Wall pyramid_felippa_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pyramid_felippa_rule_test.o $HOME/lib/pyramid_felippa_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pyramid_felippa_rule_test.o
#
mv a.out pyramid_felippa_rule_test
./pyramid_felippa_rule_test > pyramid_felippa_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pyramid_felippa_rule_test
#
echo "Normal end of execution."
