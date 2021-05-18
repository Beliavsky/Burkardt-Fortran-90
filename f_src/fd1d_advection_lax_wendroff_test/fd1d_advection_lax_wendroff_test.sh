#! /bin/bash
#
$HOME/bin/fd1d_advection_lax_wendroff > fd1d_advection_lax_wendroff_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
gnuplot < fd1d_advection_lax_wendroff_commands.txt
#
echo "Normal end of execution."
