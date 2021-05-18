#! /bin/bash
#
$HOME/bin/heated_plate 0.001 sol_500x500.txt > heated_plate_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
