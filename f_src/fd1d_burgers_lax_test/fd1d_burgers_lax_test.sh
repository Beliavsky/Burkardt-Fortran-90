#! /bin/bash
#
$HOME/bin/fd1d_burgers_lax > fd1d_burgers_lax_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
