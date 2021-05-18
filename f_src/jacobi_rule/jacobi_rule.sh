#! /bin/bash
#
gfortran -c -Wall jacobi_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran jacobi_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm jacobi_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/jacobi_rule
#
echo "Normal end of execution."
