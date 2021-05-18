#! /bin/bash
#
gfortran -c -Wall tetrahedron_integrals_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tetrahedron_integrals_test.o $HOME/lib/tetrahedron_integrals.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tetrahedron_integrals_test.o
#
mv a.out tetrahedron_integrals_test
./tetrahedron_integrals_test > tetrahedron_integrals_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm tetrahedron_integrals_test
#
echo "Normal end of execution."
