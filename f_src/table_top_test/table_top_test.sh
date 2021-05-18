#! /bin/bash
#
$HOME/bin/table_top cvt_03_00056.txt > table_top_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
ps2png cvt_03_00056.ps cvt_03_00056.png
rm cvt_03_00056.ps
#
echo "Normal end of execution."
