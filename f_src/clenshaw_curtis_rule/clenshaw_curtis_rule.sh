#! /bin/bash
#
gfortran -c -Wall clenshaw_curtis_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran clenshaw_curtis_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm clenshaw_curtis_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/clenshaw_curtis_rule
#
echo "Normal end of execution."
