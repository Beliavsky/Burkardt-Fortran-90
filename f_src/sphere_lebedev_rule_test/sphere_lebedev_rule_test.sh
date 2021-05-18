#! /bin/bash
#
gfortran -c -Wall sphere_lebedev_rule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sphere_lebedev_rule_test.o $HOME/lib/sphere_lebedev_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sphere_lebedev_rule_test.o
#
mv a.out sphere_lebedev_rule_test
./sphere_lebedev_rule_test > sphere_lebedev_rule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sphere_lebedev_rule_test
#
echo "Normal end of execution."
