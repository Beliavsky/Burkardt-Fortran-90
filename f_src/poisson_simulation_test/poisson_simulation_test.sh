#! /bin/bash
#
gfortran -c -Wall poisson_simulation_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran poisson_simulation_test.o $HOME/lib/poisson_simulation.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm poisson_simulation_test.o
#
mv a.out poisson_simulation_test
./poisson_simulation_test > poisson_simulation_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm poisson_simulation_test
#
gnuplot poisson_events_commands.txt
gnuplot poisson_times_commands.txt
gnuplot poisson_timeline_commands.txt
#
echo "Normal end of execution."
