#! /bin/bash
#
gfortran -c -Wall index_test.f90
if [ $? -ne 0 ]; then
    echo "Compile error."
  exit
fi
#
gfortran index_test.o $HOME/lib/index.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm index_test.o
#
mv a.out index_test
./index_test > index_test.txt
if [ $? -ne 0 ]; then
  echo "Errors running index_test"
  exit
fi
rm index_test
#
echo "Normal end of execution."
