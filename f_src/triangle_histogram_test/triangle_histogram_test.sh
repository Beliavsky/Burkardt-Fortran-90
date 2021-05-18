#! /bin/bash
#
$HOME/bin/triangle_histogram g10000.txt 4 > triangle_histogram_test.txt
$HOME/bin/triangle_histogram b10000.txt 4 >> triangle_histogram_test.txt
#
echo "Normal end of execution."
