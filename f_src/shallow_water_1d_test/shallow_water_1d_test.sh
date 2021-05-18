#! /bin/bash
#
$HOME/bin/shallow_water_1d > shallow_water_1d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
