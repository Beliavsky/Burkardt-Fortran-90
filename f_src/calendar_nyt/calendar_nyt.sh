#! /bin/bash
#
gfortran -c -Wall calendar_nyt.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv calendar_nyt.o ~/lib/calendar_nyt.o
#
echo "Normal end of execution."
