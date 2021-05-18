#! /bin/bash
#
gfortran -c -Wall geompack3_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran geompack3_test.o $HOME/lib/geompack3.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm geompack3_test.o
#
mv a.out geompack3_test
./geompack3_test > geompack3_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm geompack3_test
#
echo "Normal end of execution."
