#! /bin/bash
#
~/bin/tsp_brute < five_input.txt > five_output.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
