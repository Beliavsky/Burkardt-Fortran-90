#! /bin/bash
#
gfortran -c -Wall product_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran product_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm product_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/product_rule
#
echo "Normal end of execution."
