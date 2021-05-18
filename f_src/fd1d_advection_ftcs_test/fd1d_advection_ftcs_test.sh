#! /bin/bash
#
$HOME/bin/fd1d_advection_ftcs > fd1d_advection_ftcs_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
gnuplot < fd1d_advection_ftcs_commands.txt
#
echo "Normal end of execution."
