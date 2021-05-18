#! /bin/bash
#
gfortran -c -Wall hermite_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hermite_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hermite_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/hermite_rule
#
echo "Normal end of execution."
