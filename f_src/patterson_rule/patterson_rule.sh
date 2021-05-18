#! /bin/bash
#
gfortran -c -Wall patterson_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran patterson_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm patterson_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/patterson_rule
#
echo "Normal end of execution."
