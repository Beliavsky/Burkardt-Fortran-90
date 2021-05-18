#! /bin/bash
#
gfortran -c -Wall stop_message_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran stop_message_test.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm stop_message_test.o
#
mv a.out stop_message_test
./stop_message_test > stop_message_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm stop_message_test
#
echo "Normal end of execution"
