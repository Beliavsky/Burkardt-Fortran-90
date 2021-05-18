#! /bin/bash
#
gfortran -c -Wall uniform_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran uniform_test.o $HOME/lib/uniform.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm uniform_test.o
#
mv a.out uniform_test
./uniform_test > uniform_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm uniform_test
#
echo "Normal end of execution."
