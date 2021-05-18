#! /bin/bash
#
gfortran -c -Wall ornstein_uhlenbeck_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ornstein_uhlenbeck_test.o $HOME/lib/ornstein_uhlenbeck.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ornstein_uhlenbeck_test.o
#
mv a.out ornstein_uhlenbeck_test
./ornstein_uhlenbeck_test > ornstein_uhlenbeck_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ornstein_uhlenbeck_test
#
gnuplot < ou_euler_commands.txt
gnuplot < ou_euler_maruyama_commands.txt
#
echo "Normal end of execution."
