#! /bin/bash
#
$HOME/bin/triangulation_svg lake > triangulation_svg_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
