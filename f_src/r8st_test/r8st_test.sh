#! /bin/bash
#
gfortran -c -Wall r8st_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o r8st_test r8st_test.o $HOME/lib/r8st.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm r8st_test.o
#
./r8st_test > r8st_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm r8st_test
#
echo "Normal end of execution."
