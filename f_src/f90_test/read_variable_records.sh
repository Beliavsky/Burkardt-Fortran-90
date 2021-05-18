#! /bin/bash
#
gfortran -c -Wall read_variable_records.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran read_variable_records.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm read_variable_records.o
#
mv a.out read_variable_records
./read_variable_records > read_variable_records_output.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm read_variable_records
#
echo "Normal end of execution."
