#! /bin/bash
#
gfortran -c -Wall svd_basis_weight.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran svd_basis_weight.o -llapack
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm svd_basis_weight.o
#
chmod ugo+x a.out
mv a.out ~/bin/svd_basis_weight
#
echo "Normal end of execution."
