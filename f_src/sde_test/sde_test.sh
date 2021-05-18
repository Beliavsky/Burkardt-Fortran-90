#! /bin/bash
#
gfortran -c -Wall sde_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sde_test.o $HOME/lib/sde.o $HOME/lib/qr_solve.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sde_test.o
#
mv a.out sde_test
./sde_test > sde_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sde_test
#
gnuplot < bpath1_commands.txt
gnuplot < bpath_average_commands.txt
gnuplot < bpath_commands.txt
gnuplot < chain_commands.txt
gnuplot < em_commands.txt
gnuplot < emstrong_commands.txt
gnuplot < emweak0_commands.txt
gnuplot < emweak1_commands.txt
gnuplot < milstrong_commands.txt
gnuplot < stab_asymptotic_commands.txt
gnuplot < stab_meansquare_commands.txt
#
echo "Normal end of execution."
