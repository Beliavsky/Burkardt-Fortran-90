#! /bin/bash
#
$HOME/bin/triangle_to_xml ell > triangle_to_xml_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

