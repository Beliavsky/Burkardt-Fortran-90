#! /bin/bash
#
$HOME/bin/circle_packing halton_02_00010.txt >  circle_packing_test.txt
$HOME/bin/circle_packing halton_02_00100.txt >> circle_packing_test.txt
$HOME/bin/circle_packing halton_02_01000.txt >> circle_packing_test.txt
#
echo "Normal end of execution."
