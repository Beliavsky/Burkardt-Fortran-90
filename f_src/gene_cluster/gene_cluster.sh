#! /bin/bash
#
gfortran -c -Wall gene_cluster.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran gene_cluster.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm gene_cluster.o
#
mv a.out ~/bin/gene_cluster
#
echo "Normal end of execution."
