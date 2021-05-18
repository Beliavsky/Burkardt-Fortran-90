#! /bin/bash
#
echo "Run with 1 thread."
export OMP_NUM_THREADS=1
$HOME/bin/mandelbrot_openmp > mandelbrot_openmp_test.txt
#
echo "Run with 2 threads."
export OMP_NUM_THREADS=2
$HOME/bin/mandelbrot_openmp >> mandelbrot_openmp_test.txt
#
echo "Run with 4 threads."
export OMP_NUM_THREADS=4
$HOME/bin/mandelbrot_openmp >> mandelbrot_openmp_test.txt
#
echo "Normal end of execution."
