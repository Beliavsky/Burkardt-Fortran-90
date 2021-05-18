#! /bin/bash
#
gfortran -c -Wall partition_problem_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran partition_problem_test.o $HOME/lib/partition_problem.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm partition_problem_test.o
#
mv a.out partition_problem_test
./partition_problem_test > partition_problem_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm partition_problem_test
#
echo "Normal end of execution."
