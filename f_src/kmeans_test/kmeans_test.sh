#! /bin/bash
#
gfortran -c -Wall kmeans_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran kmeans_test.o $HOME/lib/kmeans.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm kmeans_test.o
#
mv a.out kmeans_test
./kmeans_test > kmeans_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm kmeans_test
#
echo "Normal end of execution."
