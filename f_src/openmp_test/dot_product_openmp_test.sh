#! /bin/bash
#
gfortran -o dot_product_openmp_test -Wall -fopenmp dot_product_openmp_test.f90
#
rm -f dot_product_openmp_test.txt
#
for threads in 1 2 4 8
do
  echo "Run with "$threads" threads."
  export OMP_NUM_THREADS=$threads
  ./dot_product_openmp_test >> dot_product_openmp_test.txt
  if [ $? -ne 0 ]; then
    echo "Run error."
    exit
  fi
done
#
rm dot_product_openmp_test
#
echo "Normal end of execution."
