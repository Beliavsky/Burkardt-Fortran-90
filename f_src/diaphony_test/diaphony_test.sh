#! /bin/bash
#
$HOME/bin/diaphony cvt_02_00100.txt > diaphony_test.txt
$HOME/bin/diaphony halton_02_00100.txt >> diaphony_test.txt
$HOME/bin/diaphony uniform_02_00010.txt >> diaphony_test.txt
$HOME/bin/diaphony uniform_02_00100.txt >> diaphony_test.txt
$HOME/bin/diaphony uniform_07_10000.txt >> diaphony_test.txt
#
echo "Normal end of execution."
