#! /bin/bash
#
gfortran -c -Wall ccn_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ccn_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ccn_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/ccn_rule
#
echo "Normal end of execution."
