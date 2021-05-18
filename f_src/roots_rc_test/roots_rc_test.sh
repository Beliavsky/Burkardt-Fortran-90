#! /bin/bash
#
gfortran -c -Wall roots_rc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran roots_rc_test.o $HOME/lib/roots_rc.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm roots_rc_test.o
#
mv a.out roots_rc_test
./roots_rc_test > roots_rc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm roots_rc_test
#
echo "Normal end of execution."
