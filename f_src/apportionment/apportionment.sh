#! /bin/bash
#
gfortran -c -Wall apportionment.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv apportionment.o ~/lib/apportionment.o
#
echo "Normal end of execution."
