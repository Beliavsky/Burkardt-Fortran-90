#!/bin/bash
#
gfortran -c timer_system_clock.f90
if [ $? -ne 0 ]; then
  echo "Errors compiling timer_system_clock.f90"
  exit
fi
#
gfortran timer_system_clock.o
if [ $? -ne 0 ]; then
  echo "Errors loading timer_system_clock.o"
  exit
fi
rm timer_system_clock.o
#
mv a.out timer_system_clock
./timer_system_clock > timer_system_clock_output.txt
if [ $? -ne 0 ]; then
  echo "Errors running timer_system_clock"
  exit
fi
rm timer_system_clock
#
echo "Program output written to timer_system_clock_output.txt."
