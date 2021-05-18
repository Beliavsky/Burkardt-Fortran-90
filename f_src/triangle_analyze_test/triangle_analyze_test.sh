#! /bin/sh
#
$HOME/bin/triangle_analyze tex1.txt > tex1_properties.txt
$HOME/bin/triangle_analyze tex2.txt > tex2_properties.txt
$HOME/bin/triangle_analyze tex3.txt > tex3_properties.txt
$HOME/bin/triangle_analyze tex4.txt > tex4_properties.txt
$HOME/bin/triangle_analyze tex5.txt > tex5_properties.txt
$HOME/bin/triangle_analyze tex6.txt > tex6_properties.txt
$HOME/bin/triangle_analyze tex7.txt > tex7_properties.txt
$HOME/bin/triangle_analyze tex8.txt > tex8_properties.txt
$HOME/bin/triangle_analyze triangle_equilateral.txt > triangular_equilateral_properties.txt
$HOME/bin/triangle_analyze triangle_needle.txt > triangular_needle_properties.txt
$HOME/bin/triangle_analyze triangle_ref.txt > triangular_ref_properties.txt
$HOME/bin/triangle_analyze triangle_right.txt > triangular_right_properties.txt
#
echo "Normal end of execution."

