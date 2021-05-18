#! /bin/bash
#
$HOME/bin/hyperball_volume_monte_carlo 6 987654321 > hyperball_volume_monte_carlo_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
