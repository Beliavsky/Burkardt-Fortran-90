#! /bin/bash
#
gfortran -c -Wall walsh_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran walsh_test.o $HOME/lib/walsh.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm walsh_test.o
#
mv a.out walsh_test
./walsh_test > walsh_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm walsh_test
#
echo "Normal end of execution."
