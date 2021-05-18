#! /bin/bash
#
gfortran -c -Wall task_division_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran task_division_test.o $HOME/lib/task_division.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm task_division_test.o
#
mv a.out task_division_test
./task_division_test > task_division_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm task_division_test
#
echo "Normal end of execution."
