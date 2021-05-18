#! /bin/bash
#
gfortran -c -Wall ketel_one.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o ketel_one ketel_one.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ketel_one.o
#
mv ketel_one ~/bin/ketel_one
#
echo "Normal end of execution."
