#! /bin/bash
#
gfortran -c -Wall ccs_to_st.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ccs_to_st.o ~/lib/ccs_to_st.o
#
echo "Normal end of execution."
