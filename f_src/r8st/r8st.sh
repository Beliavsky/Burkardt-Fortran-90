#! /bin/bash
#
gfortran -c -Wall r8st.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv r8st.o ~/lib/r8st.o
#
echo "Normal end of execution."
