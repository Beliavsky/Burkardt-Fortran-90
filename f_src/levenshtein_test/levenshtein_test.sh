#! /bin/bash
#
gfortran -c -Wall levenshtein_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o levenshtein_test levenshtein_test.o $HOME/lib/levenshtein.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm levenshtein_test.o
#
./levenshtein_test > levenshtein_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm levenshtein_test
#
echo "Normal end of execution."
