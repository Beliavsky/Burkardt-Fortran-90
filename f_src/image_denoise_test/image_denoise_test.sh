#! /bin/bash
#
gfortran -c -Wall image_denoise_test.f90
if [ $? -ne 0 ]; then
    echo "Compile error."
  exit
fi
#
gfortran image_denoise_test.o $HOME/lib/image_denoise.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm image_denoise_test.o
#
mv a.out image_denoise_test
./image_denoise_test > image_denoise_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm image_denoise_test
#
echo "Normal end of execution."
