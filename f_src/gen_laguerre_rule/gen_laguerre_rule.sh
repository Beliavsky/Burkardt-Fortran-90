#! /bin/bash
#
gfortran -c -Wall gen_laguerre_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran gen_laguerre_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm gen_laguerre_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/gen_laguerre_rule
#
echo "Normal end of execution."
