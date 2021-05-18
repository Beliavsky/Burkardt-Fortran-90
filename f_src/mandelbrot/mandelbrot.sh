#! /bin/bash
#
gfortran -c -Wall mandelbrot.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran mandelbrot.o
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm mandelbrot.o
#
mv a.out ~/bin/mandelbrot
#
echo "Normal end of execution."
