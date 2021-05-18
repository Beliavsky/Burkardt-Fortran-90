#! /bin/bash
#
gfortran -c -Wall triangle_to_xml.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran triangle_to_xml.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm triangle_to_xml.o
#
chmod ugo+x a.out
mv a.out ~/bin/triangle_to_xml
#
echo "Normal end of execution."
