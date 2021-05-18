#! /bin/bash
#
gfortran -c -Wall line_cvt_lloyd_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o line_cvt_lloyd_test line_cvt_lloyd_test.o $HOME/lib/line_cvt_lloyd.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm line_cvt_lloyd_test.o
#
./line_cvt_lloyd_test > line_cvt_lloyd_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm line_cvt_lloyd_test
#
gnuplot < test01_energy_commands.txt
gnuplot < test01_evolution_commands.txt
gnuplot < test01_motion_commands.txt
#
gnuplot < test02_energy_commands.txt
gnuplot < test02_evolution_commands.txt
gnuplot < test02_motion_commands.txt
#
gnuplot < test03_energy_commands.txt
gnuplot < test03_evolution_commands.txt
gnuplot < test03_motion_commands.txt
#
gnuplot < test04_energy_commands.txt
gnuplot < test04_evolution_commands.txt
gnuplot < test04_motion_commands.txt
#
echo "Normal end of execution."
