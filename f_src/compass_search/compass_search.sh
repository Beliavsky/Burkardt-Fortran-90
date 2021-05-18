#! /bin/bash
#
gfortran -c -Wall compass_search.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv compass_search.o ~/lib/compass_search.o
#
echo "Normal end of execution."
