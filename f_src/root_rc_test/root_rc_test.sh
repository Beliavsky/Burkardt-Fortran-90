#! /bin/bash
#
gfortran -c -Wall root_rc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran root_rc_test.o $HOME/lib/root_rc.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm root_rc_test.o
#
mv a.out root_rc_test
./root_rc_test > root_rc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm root_rc_test
#
echo "Normal end of execution."
