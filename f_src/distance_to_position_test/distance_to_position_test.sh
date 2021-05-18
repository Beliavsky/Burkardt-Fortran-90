#! /bin/bash
#
$HOME/bin/distance_to_position grid04_dist.txt > distance_to_position_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
