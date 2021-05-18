#! /bin/bash
#
$HOME/bin/triangulation_q2l example > triangulation_q2l_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
