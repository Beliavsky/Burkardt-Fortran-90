#! /bin/bash
#
gfortran -c -pg -Wall linpack_bench.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -pg linpack_bench.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm linpack_bench.o
#
mv a.out linpack_bench
#
./linpack_bench > linpack_bench.txt
#
gprof linpack_bench > gprof_test.txt
#
#  Clean up.
#  GPROF creates a temporary file GMON.OUT that we don't need.
#
rm linpack_bench
rm gmon.out
#
echo "Normal end of execution."
