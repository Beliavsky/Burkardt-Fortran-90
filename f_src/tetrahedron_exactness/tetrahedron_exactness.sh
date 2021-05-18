#! /bin/bash
#
gfortran -c -Wall tetrahedron_exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tetrahedron_exactness.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tetrahedron_exactness.o
#
chmod ugo+x a.out
mv a.out ~/bin/tetrahedron_exactness
#
echo "Normal end of execution."
