#! /bin/bash
#
gfortran -c -Wall file_name_sequence_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran file_name_sequence_test.o $HOME/lib/file_name_sequence.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm file_name_sequence_test.o
#
mv a.out file_name_sequence_test
./file_name_sequence_test > file_name_sequence_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm file_name_sequence_test
#
echo "Normal end of execution."
