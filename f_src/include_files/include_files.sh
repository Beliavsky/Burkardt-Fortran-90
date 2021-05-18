#! /bin/bash
#
gfortran -c -Wall include_files.f90
if [ $? -ne 0 ]; then
    echo "Compile error."
  exit
fi
#
gfortran include_files.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm include_files.o
#
chmod ugo+x a.out
mv a.out ~/bin/include_files
#
echo "Normal end of execution."
