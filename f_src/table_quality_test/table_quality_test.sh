#! /bin/bash
#
$HOME/bin/table_quality cvt_02_00100.txt > table_quality_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
