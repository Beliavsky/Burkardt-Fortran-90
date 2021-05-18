#! /bin/bash
#
$HOME/bin/fem1d_sample p1_fem p1_sample > fem1d_sample_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
$HOME/bin/fem1d_sample p2_fem p2_sample >> fem1d_sample_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
