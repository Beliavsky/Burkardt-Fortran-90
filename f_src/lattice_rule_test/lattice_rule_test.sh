#! /bin/bash
#
gfortran -c -Wall lattice_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran lattice_rule_test.o $HOME/lib/lattice_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lattice_rule_test.o
#
mv a.out lattice_rule_test
./lattice_rule_test > lattice_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm lattice_rule_test
#
echo "Normal end of execution."
