#! /bin/bash
#
gfortran -c -Wall discrete_pdf_sample_2d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran discrete_pdf_sample_2d.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm discrete_pdf_sample_2d.o
#
mv a.out ~/bin/discrete_pdf_sample_2d
#
echo "Normal end of execution."
