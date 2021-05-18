#! /bin/bash
#
gfortran -c -Wall wishart_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran wishart_test.o $HOME/lib/wishart.o $HOME/lib/pdflib.o $HOME/lib/rnglib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm wishart_test.o
#
mv a.out wishart_test
./wishart_test > wishart_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm wishart_test
#
echo "Normal end of execution."
