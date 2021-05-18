#! /bin/bash
#
gfortran -c -Wall tetrahedron_monte_carlo_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tetrahedron_monte_carlo_test.o $HOME/lib/tetrahedron_monte_carlo.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tetrahedron_monte_carlo_test.o
#
mv a.out tetrahedron_monte_carlo_test
./tetrahedron_monte_carlo_test > tetrahedron_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm tetrahedron_monte_carlo_test
#
echo "Normal end of execution."
