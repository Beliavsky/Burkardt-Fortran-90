#! /bin/bash
#
gfortran -c -Wall pdflib.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv pdflib.o ~/lib/pdflib.o
#
echo "Normal end of execution."
