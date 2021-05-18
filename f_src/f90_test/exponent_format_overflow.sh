#! /bin/bash
#
gfortran -c -Wall exponent_format_overflow.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran exponent_format_overflow.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm exponent_format_overflow.o
#
mv a.out exponent_format_overflow
./exponent_format_overflow > exponent_format_overflow.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm exponent_format_overflow
#
echo "Normal end of execution."
