#! /bin/bash
#
gfortran -c -Wall triangle_ncc_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangle_ncc_rule_test.o $HOME/lib/triangle_ncc_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle_ncc_rule_test.o
#
mv a.out triangle_ncc_rule_test
./triangle_ncc_rule_test > triangle_ncc_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm triangle_ncc_rule_test
#
echo "Normal end of execution."
