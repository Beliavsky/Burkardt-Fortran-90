#! /bin/bash
#
gfortran -c -Wall st_to_ccs_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o st_to_ccs_test st_to_ccs_test.o $HOME/lib/st_to_ccs.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm st_to_ccs_test.o
#
./st_to_ccs_test > st_to_ccs_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm st_to_ccs_test
#
echo "Normal end of execution."
