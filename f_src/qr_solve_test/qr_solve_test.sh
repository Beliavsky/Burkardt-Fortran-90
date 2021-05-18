#! /bin/bash
#
gfortran -c -Wall qr_solve_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran qr_solve_test.o $HOME/lib/qr_solve.o $HOME/lib/test_lls.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm qr_solve_test.o
#
mv a.out qr_solve_test
./qr_solve_test > qr_solve_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm qr_solve_test
#
echo "Normal end of execution."
