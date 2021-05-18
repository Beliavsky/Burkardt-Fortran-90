#! /bin/bash
#
gfortran -c -Wall test_opt_con_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_opt_con_test.o $HOME/lib/test_opt_con.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_opt_con_test.o
#
mv a.out test_opt_con_test
./test_opt_con_test > test_opt_con_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_opt_con_test
#
echo "Normal end of execution."
