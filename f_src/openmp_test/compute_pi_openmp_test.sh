#! /bin/bash
#
gfortran -o compute_pi_openmp_test -Wall -fopenmp compute_pi_openmp_test.f90
#
rm -f compute_pi_openmp_test.txt
#
for threads in 1 2 4 8
do
  echo "Run with "$threads" threads."
  export OMP_NUM_THREADS=$threads
  ./compute_pi_openmp_test >> compute_pi_openmp_test.txt
  if [ $? -ne 0 ]; then
    echo "Run error."
    exit
  fi
done
#
rm compute_pi_openmp_test
#
echo "Normal end of execution."
