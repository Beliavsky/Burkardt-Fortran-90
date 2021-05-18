#! /bin/bash
#
gfortran -c -Wall linpack_bench.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran linpack_bench.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm linpack_bench.o
#
mv a.out ~/bin/linpack_bench
#
echo "Normal end of execution."
