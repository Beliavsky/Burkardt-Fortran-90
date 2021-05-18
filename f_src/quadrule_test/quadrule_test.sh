#! /bin/bash
#
gfortran -c -Wall quadrule_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran quadrule_test.o $HOME/lib/quadrule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quadrule_test.o
#
mv a.out quadrule_test
./quadrule_test > quadrule_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm quadrule_test
#
echo "Normal end of execution."
