#! /bin/bash
#
gfortran -c -Wall triangle_twb_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o triangle_twb_rule_test triangle_twb_rule_test.o /$HOME/lib/triangle_twb_rule.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm triangle_twb_rule_test.o
#
./triangle_twb_rule_test > triangle_twb_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm triangle_twb_rule_test
#
echo "Normal end of execution."
