#! /bin/bash
#
gfortran -c -Wall line_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran line_integrals_test.o $HOME/lib/line_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm line_integrals_test.o
#
mv a.out line_integrals_test
./line_integrals_test > line_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm line_integrals_test
#
echo "Normal end of execution."
