#! /bin/bash
#
$HOME/bin/life_serial > life_serial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

