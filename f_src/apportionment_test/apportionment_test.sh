#! /bin/bash
#
gfortran -c -Wall apportionment_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o apportionment_test apportionment_test.o /$HOME/lib/apportionment.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm apportionment_test.o
#
./apportionment_test > apportionment_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm apportionment_test
#
echo "Normal end of execution."
