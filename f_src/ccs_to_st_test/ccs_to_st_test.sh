#! /bin/bash
#
gfortran -c -Wall ccs_to_st_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o ccs_to_st_test ccs_to_st_test.o $HOME/lib/ccs_to_st.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ccs_to_st_test.o
#
./ccs_to_st_test > ccs_to_st_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm ccs_to_st_test
#
echo "Normal end of execution."
