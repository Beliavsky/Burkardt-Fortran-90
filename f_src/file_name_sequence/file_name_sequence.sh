#! /bin/bash
#
gfortran -c -Wall file_name_sequence.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv file_name_sequence.o ~/lib/file_name_sequence.o
#
echo "Normal end of execution."
