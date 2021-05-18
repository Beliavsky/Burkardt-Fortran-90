#! /bin/bash
#
gfortran -c -Wall image_edge_test.f90
if [ $? -ne 0 ]; then
    echo "Compile error."
  exit
fi
#
gfortran image_edge_test.o $HOME/lib/image_edge.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm image_edge_test.o
#
mv a.out image_edge_test
./image_edge_test > image_edge_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm image_edge_test
#
echo "Normal end of execution."
