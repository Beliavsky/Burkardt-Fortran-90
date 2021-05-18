#! /bin/bash
#
gfortran -c -Wall legendre_rule_fast.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran legendre_rule_fast.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm legendre_rule_fast.o
#
chmod ugo+x a.out
mv a.out ~/bin/legendre_rule_fast
#
echo "Normal end of execution."
