#! /bin/bash
#
gfortran -c -Wall gegenbauer_cc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o gegenbauer_cc_test gegenbauer_cc_test.o $HOME/lib/gegenbauer_cc.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm gegenbauer_cc_test.o
#
./gegenbauer_cc_test > gegenbauer_cc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm gegenbauer_cc_test
#
echo "Normal end of execution."
