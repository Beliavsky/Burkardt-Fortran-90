#! /bin/bash
#
gfortran -c -Wall tetrahedron_arbq_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tetrahedron_arbq_rule_test.o $HOME/lib/tetrahedron_arbq_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tetrahedron_arbq_rule_test.o
#
mv a.out tetrahedron_arbq_rule_test
./tetrahedron_arbq_rule_test > tetrahedron_arbq_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm tetrahedron_arbq_rule_test
#
echo "Normal end of execution."
