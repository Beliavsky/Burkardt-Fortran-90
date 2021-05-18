#! /bin/bash
#
$HOME/bin/triangulation_l2q example > triangulation_l2q_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
