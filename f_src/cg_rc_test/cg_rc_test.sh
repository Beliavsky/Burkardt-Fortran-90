#! /bin/bash
#
gfortran -c -Wall cg_rc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cg_rc_test cg_rc_test.o $HOME/lib/cg_rc.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cg_rc_test.o
#
./cg_rc_test > cg_rc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cg_rc_test
#
echo "Normal end of execution."
