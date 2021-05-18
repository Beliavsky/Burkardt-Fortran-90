#! /bin/bash
#
$HOME/bin/files_multiple > files_multiple_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
