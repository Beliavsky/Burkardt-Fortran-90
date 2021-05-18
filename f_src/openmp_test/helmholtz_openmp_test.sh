#! /bin/bash
#
gfortran -o helmholtz_openmp_test -Wall -fopenmp helmholtz_openmp_test.f90
#
rm -f helmholtz_openmp_test.txt
#
for threads in 1 2 4 8
do
  echo "Run with "$threads" threads."
  export OMP_NUM_THREADS=$threads
  ./helmholtz_openmp_test >> helmholtz_openmp_test.txt
  if [ $? -ne 0 ]; then
    echo "Run error."
    exit
  fi
done
#
rm helmholtz_openmp_test
#
echo "Normal end of execution."
