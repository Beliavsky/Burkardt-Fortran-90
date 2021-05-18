#! /bin/bash
#
gfortran -c -Wall high_card_simulation_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran high_card_simulation_test.o $HOME/lib/high_card_simulation.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm high_card_simulation_test.o
#
mv a.out high_card_simulation_test
./high_card_simulation_test > high_card_simulation_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm high_card_simulation_test
#
gnuplot < test02_commands.txt
gnuplot < test03_commands.txt
#
echo "Normal end of execution."
