#! /bin/bash
#
gfortran -c -Wall r8ge_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran r8ge_test.o $HOME/lib/r8ge.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm r8ge_test.o
#
mv a.out r8ge_test
./r8ge_test > r8ge_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm r8ge_test
#
echo "Normal end of execution."
