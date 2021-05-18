#! /bin/bash
#
$HOME/bin/meshless < input.txt > meshless_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
ps2png basis.eps basis.png
#
echo "Normal end of execution."
