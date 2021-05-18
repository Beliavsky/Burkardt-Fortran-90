#! /bin/bash
#
gfortran -c -Wall table_quality.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_quality.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_quality.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_quality
#
echo "Normal end of execution."
