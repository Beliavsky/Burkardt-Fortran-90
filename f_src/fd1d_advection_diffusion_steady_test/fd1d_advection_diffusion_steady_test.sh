#! /bin/bash
#
$HOME/bin/fd1d_advection_diffusion_steady > fd1d_advection_diffusion_steady_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
gnuplot < fd1d_advection_diffusion_steady_commands.txt
#
echo "Normal end of execution."
