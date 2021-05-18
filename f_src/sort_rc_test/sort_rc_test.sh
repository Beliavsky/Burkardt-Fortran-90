#! /bin/bash
#
gfortran -c -Wall sort_rc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o sort_rc_test sort_rc_test.o $HOME/lib/sort_rc.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sort_rc_test.o
#
./sort_rc_test > sort_rc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sort_rc_test
#
echo "Normal end of execution."
