#! /bin/bash
#
gfortran -c -Wall backtrack_binary_rc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o backtrack_binary_rc_test backtrack_binary_rc_test.o $HOME/lib/backtrack_binary_rc.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm backtrack_binary_rc_test.o
#
./backtrack_binary_rc_test > backtrack_binary_rc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm backtrack_binary_rc_test
#
echo "Normal end of execution."
