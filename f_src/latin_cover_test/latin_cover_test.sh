#! /bin/bash
#
gfortran -c -Wall latin_cover_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran latin_cover_test.o $HOME/lib/latin_cover.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm latin_cover_test.o
#
mv a.out latin_cover_test
./latin_cover_test > latin_cover_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm latin_cover_test
#
echo "Normal end of execution."
