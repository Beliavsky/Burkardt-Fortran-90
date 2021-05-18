#! /bin/bash
#
gfortran -c -Wall toms672_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o toms672_test toms672_test.o $HOME/lib/toms672.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm toms672_test.o
#
./toms672_test < input.txt > toms672_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm toms672_test
#
echo "Normal end of execution."
