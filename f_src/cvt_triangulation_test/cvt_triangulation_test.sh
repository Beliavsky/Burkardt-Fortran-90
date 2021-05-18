#! /bin/bash
#
$HOME/bin/cvt_triangulation > cvt_triangulation_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
