#! /bin/bash
#
gfortran -c -Wall graph_paper_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran graph_paper_test.o $HOME/lib/graph_paper.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm graph_paper_test.o
#
mv a.out graph_paper_test
./graph_paper_test > graph_paper_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm graph_paper_test
#
ps2png hexagonal_1.eps hexagonal_1.png
rm hexagonal_1.eps
#
ps2png hexagonal_2.eps hexagonal_2.png
rm hexagonal_2.eps
#
ps2png hexagonal_3.eps hexagonal_3.png
rm hexagonal_3.eps
#
ps2png hexagonal_4.eps hexagonal_4.png
rm hexagonal_4.eps
#
ps2png hexagonal_5.eps hexagonal_5.png
rm hexagonal_5.eps
#
ps2png polar_1.eps polar_1.png
rm polar_1.eps
#
ps2png sudoku_blank.eps sudoku_blank.png
rm sudoku_blank.eps
#
ps2png sudoku_filled.eps sudoku_filled.png
rm sudoku_filled.eps
#
ps2png staggered_2.eps staggered_2.png
rm staggered_2.eps
#
ps2png triangular_1.eps triangular_1.png
rm triangular_1.eps
#
ps2png triangular_2.eps triangular_2.png
rm triangular_2.eps
#
ps2png uniform_1.eps uniform_1.png
rm uniform_1.eps
#
ps2png uniform_2.eps uniform_2.png
rm uniform_2.eps
#
ps2png uniform_3.eps uniform_3.png
rm uniform_3.eps
#
ps2png uniform_4.eps uniform_4.png
rm uniform_4.eps
#
echo "Normal end of execution."

