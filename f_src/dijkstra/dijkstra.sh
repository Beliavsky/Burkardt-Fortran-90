#! /bin/bash
#
gfortran -c -Wall dijkstra.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran dijkstra.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm dijkstra.o
#
chmod ugo+x a.out
mv a.out $HOME/bin/dijkstra
#
echo "Normal end of execution."
