#! /bin/bash
#
gfortran -c -Wall praxis_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o praxis_test praxis_test.o /$HOME/lib/praxis.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm praxis_test.o
#
./praxis_test > praxis_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm praxis_test
#
echo "Normal end of execution."
