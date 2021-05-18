#! /bin/bash
#
$HOME/bin/table_columns 2 1 2 2 input.txt output2122.txt > table_columns_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
