#! /bin/bash
#
gfortran -c -Wall pyramid_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pyramid_integrals_test.o $HOME/lib/pyramid_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pyramid_integrals_test.o
#
mv a.out pyramid_integrals_test
./pyramid_integrals_test > pyramid_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pyramid_integrals_test
#
echo "Normal end of execution."
