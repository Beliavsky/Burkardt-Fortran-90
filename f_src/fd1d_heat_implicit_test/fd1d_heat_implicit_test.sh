#! /bin/bash
#
$HOME/bin/fd1d_heat_implicit > fd1d_heat_implicit_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
