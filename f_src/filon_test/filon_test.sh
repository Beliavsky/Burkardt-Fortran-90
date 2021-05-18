#! /bin/bash
#
gfortran -c -Wall filon_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran filon_test.o $HOME/lib/filon.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm filon_test.o
#
mv a.out filon_test
./filon_test > filon_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm filon_test
#
echo "Normal end of execution."
