#! /bin/bash
#
gfortran -c -Wall newton_rc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran newton_rc_test.o /$HOME/lib/newton_rc.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm newton_rc_test.o
#
mv a.out newton_rc_test
./newton_rc_test > newton_rc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm newton_rc_test
#
echo "Normal end of execution."
