#! /bin/bash
#
gfortran -c -Wall chebyshev1_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran chebyshev1_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm chebyshev1_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/chebyshev1_rule
#
echo "Normal end of execution."
