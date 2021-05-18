#! /bin/bash
#
$HOME/bin/include_files file1.for file2.for > include_files_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
