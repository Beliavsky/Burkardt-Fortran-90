#! /bin/bash
#
gfortran -c -Wall multigrid_poisson_1d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran multigrid_poisson_1d_test.o $HOME/lib/multigrid_poisson_1d.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm multigrid_poisson_1d_test.o
#
mv a.out multigrid_poisson_1d_test
./multigrid_poisson_1d_test > multigrid_poisson_1d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm multigrid_poisson_1d_test
#
echo "Normal end of execution."
