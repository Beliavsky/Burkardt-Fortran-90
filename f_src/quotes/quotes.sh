#! /bin/bash
#
gfortran -c -Wall quotes.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran quotes.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quotes.o
#
mv a.out ~/bin/quotes
#
echo "Normal end of execution."
