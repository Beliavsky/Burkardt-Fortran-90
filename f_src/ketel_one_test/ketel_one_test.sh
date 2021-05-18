#! /bin/bash
#
$HOME/bin/ketel_one > ketel_one_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

