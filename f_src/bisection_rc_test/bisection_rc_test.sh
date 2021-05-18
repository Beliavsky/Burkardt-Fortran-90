#! /bin/bash
#
gfortran -c -Wall bisection_rc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o bisection_rc_test bisection_rc_test.o $HOME/lib/bisection_rc.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bisection_rc_test.o
#
./bisection_rc_test > bisection_rc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bisection_rc_test
#
echo "Normal end of execution."
