#! /bin/bash
#
$HOME/bin/file_transpose pigs.txt pigs_transposed.txt      > file_transpose_test.txt
$HOME/bin/file_transpose matrix.txt matrix_transposed.txt >> file_transpose_test.txt
$HOME/bin/file_transpose story.txt story_transposed.txt   >> file_transpose_test.txt
#
echo "Normal end of execution."
