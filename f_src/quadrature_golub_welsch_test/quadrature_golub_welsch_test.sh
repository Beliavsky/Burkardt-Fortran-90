#! /bin/bash
#
gfortran -c -Wall quadrature_golub_welsch_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran quadrature_golub_welsch_test.o $HOME/lib/quadrature_golub_welsch.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quadrature_golub_welsch_test.o
#
mv a.out quadrature_golub_welsch_test
./quadrature_golub_welsch_test > quadrature_golub_welsch_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm quadrature_golub_welsch_test
#
echo "Normal end of execution."
