#! /bin/bash
#
$HOME/bin/mandelbrot > mandelbrot_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
convert mandelbrot.ppm mandelbrot.png
#
echo "Normal end of execution."
