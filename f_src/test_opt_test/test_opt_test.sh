#! /bin/bash
#
gfortran -c -Wall test_opt_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_opt_test.o $HOME/lib/test_opt.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_opt_test.o
#
mv a.out test_opt_test
./test_opt_test > test_opt_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_opt_test
#
echo "Normal end of execution."
