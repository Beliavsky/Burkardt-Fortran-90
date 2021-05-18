#! /bin/bash
#
gfortran -c -Wall sge_mod.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -c sge_mod_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sge_mod_test.o sge_mod.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sge_mod_test.o
rm sge_mod.o
#
mv a.out sge_mod_test
./sge_mod_test > sge_mod_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sge_mod_test
#
echo "Normal end of execution."
