#! /bin/bash
#
$HOME/bin/fem_to_xml battery > fem_to_xml_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
$HOME/bin/fem_to_xml channel >> fem_to_xml_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
#
$HOME/bin/fem_to_xml cheby9 >> fem_to_xml_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
$HOME/bin/fem_to_xml circle >> fem_to_xml_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
$HOME/bin/fem_to_xml rectangle >> fem_to_xml_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
