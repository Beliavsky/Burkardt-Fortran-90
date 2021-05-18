#! /bin/bash
#
gfortran -c -Wall polyominoes_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o polyominoes_test polyominoes_test.o $HOME/lib/polyominoes.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm polyominoes_test.o
#
./polyominoes_test > polyominoes_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm polyominoes_test
#
echo "Normal end of execution."
