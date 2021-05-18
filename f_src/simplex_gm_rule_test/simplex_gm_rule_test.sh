#! /bin/bash
#
gfortran -c -Wall simplex_gm_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran simplex_gm_rule_test.o $HOME/lib/simplex_gm_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm simplex_gm_rule_test.o
#
mv a.out simplex_gm_rule_test
./simplex_gm_rule_test > simplex_gm_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm simplex_gm_rule_test
#
echo "Normal end of execution."
