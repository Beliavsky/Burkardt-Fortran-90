#! /bin/bash
#
$HOME/bin/histogram_data_2d_sample > histogram_data_2d_sample_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
