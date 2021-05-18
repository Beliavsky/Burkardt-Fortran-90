#! /bin/bash
#
gfortran -c -Wall chebyshev2_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran chebyshev2_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm chebyshev2_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/chebyshev2_rule
#
echo "Normal end of execution."
