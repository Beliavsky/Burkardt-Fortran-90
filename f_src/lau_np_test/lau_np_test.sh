#! /bin/bash
#
gfortran -c -Wall lau_np_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran lau_np_test.o $HOME/lib/lau_np.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lau_np_test.o
#
mv a.out lau_np_test
./lau_np_test > lau_np_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm lau_np_test
#
echo "Normal end of execution."
