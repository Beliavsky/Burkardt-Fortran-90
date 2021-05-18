#! /bin/bash
#
gfortran -c -Wall compass_search_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o compass_search_test compass_search_test.o $HOME/lib/compass_search.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm compass_search_test.o
#
./compass_search_test > compass_search_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm compass_search_test
#
echo "Normal end of execution."
