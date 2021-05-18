#! /bin/bash
#
gfortran -c -Wall lebesgue_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran lebesgue_test.o $HOME/lib/lebesgue.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lebesgue_test.o
#
mv a.out lebesgue_test
./lebesgue_test > lebesgue_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm lebesgue_test
#
echo "Normal end of execution."
