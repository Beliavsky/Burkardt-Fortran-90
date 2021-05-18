#! /bin/bash
#
gfortran -o maximum_openmp_test -Wall -fopenmp maximum_openmp_test.f90
#
rm -f maximum_openmp_test.txt
#
for threads in 1 2 4 8
do
  echo "Run with "$threads" threads."
  export OMP_NUM_THREADS=$threads
  ./maximum_openmp_test >> maximum_openmp_test.txt
  if [ $? -ne 0 ]; then
    echo "Run error."
    exit
  fi
done
#
rm maximum_openmp_test
#
echo "Normal end of execution."
