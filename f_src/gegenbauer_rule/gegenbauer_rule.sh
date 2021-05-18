#! /bin/bash
#
gfortran -c -Wall gegenbauer_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran gegenbauer_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm gegenbauer_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/gegenbauer_rule
#
echo "Normal end of execution."
