#! /bin/bash
#
gfortran -c -Wall pdflib_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o pdflib_test pdflib_test.o $HOME/lib/pdflib.o $HOME/lib/rnglib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pdflib_test.o
#
./pdflib_test > pdflib_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pdflib_test
#
echo "Normal end of execution."
