#! /bin/bash
#
gfortran -c -Wall machar_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran machar_test.o $HOME/lib/machar.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm machar_test.o
#
mv a.out machar_test
./machar_test > machar_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm machar_test
#
echo "Normal end of execution."
