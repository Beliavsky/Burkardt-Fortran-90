#! /bin/bash
#
gfortran -c -Wall local_min_rc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran local_min_rc_test.o $HOME/lib/local_min_rc.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm local_min_rc_test.o
#
mv a.out local_min_rc_test
./local_min_rc_test > local_min_rc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm local_min_rc_test
#
echo "Normal end of execution."
