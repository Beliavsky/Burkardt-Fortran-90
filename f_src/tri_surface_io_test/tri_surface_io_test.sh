#! /bin/bash
#
gfortran -c -Wall tri_surface_io_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tri_surface_io_test.o $HOME/lib/tri_surface_io.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tri_surface_io_test.o
#
mv a.out tri_surface_io_test
./tri_surface_io_test > tri_surface_io_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm tri_surface_io_test
#
echo "Normal end of execution."
