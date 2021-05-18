#! /bin/bash
#
gfortran -c -Wall table_columns.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_columns.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_columns.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_columns
#
echo "Normal end of execution."
