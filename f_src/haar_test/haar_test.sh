#! /bin/bash
#
gfortran -c -Wall haar_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran haar_test.o $HOME/lib/haar.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm haar_test.o
#
mv a.out haar_test
./haar_test > haar_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm haar_test
#
echo "Normal end of execution."
