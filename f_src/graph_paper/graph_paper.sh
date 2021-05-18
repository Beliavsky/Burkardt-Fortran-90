#! /bin/bash
#
gfortran -c -Wall graph_paper.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv graph_paper.o ~/lib/graph_paper.o
#
echo "Normal end of execution."
