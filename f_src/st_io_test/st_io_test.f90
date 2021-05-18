program main

!*****************************************************************************80
!
!! MAIN is the main program for ST_IO_TEST.
!
!  Discussion:
!
!    ST_IO_TEST tests the ST_IO library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ST_IO_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the ST_IO library.'
!
!  Double precision real tests.
!
  call r8st_write_test ( )
  call r8st_read_test ( )
  call r8st_sort_a_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ST_IO_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine r8st_write_test ( )

!*****************************************************************************80
!
!! R8ST_WRITE_TEST tests R8ST_WRITE.
!
!  Discussion:
!
!    The matrix is:
!
!      11  12   0   0  15
!      21  22   0   0   0
!       0   0  33   0  35
!       0   0   0  44   0
!      51   0  53   0  55
!
!    The index vectors are 1 based, and so have to be converted to
!    0-base before being written.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 October 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: nst = 11

  real ( kind = 8 ), dimension ( nst ) :: ast = (/ &
    51.0D+00, 12.0D+00, 11.0D+00, 33.0D+00, 15.0D+00, &
    53.0D+00, 55.0D+00, 22.0D+00, 35.0D+00, 44.0D+00, &
    21.0D+00 /)
  integer ( kind = 4 ) i_max
  integer ( kind = 4 ) i_min
  integer ( kind = 4 ) i4vec_max
  integer ( kind = 4 ) i4vec_min
  integer ( kind = 4 ), dimension ( nst ) :: ist = (/ &
     5, 1, 1, 3, 1, 5, 5, 2, 3, 4, 2 /)
  integer ( kind = 4 ) j_max
  integer ( kind = 4 ) j_min
  integer ( kind = 4 ), dimension ( nst ) :: jst = (/ &
     1, 2, 1, 3, 5, 3, 5, 2, 5, 4, 1 /)
  character ( len = 255 ) :: output_filename = 'a5by5_r8.st'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ST_WRITE_TEST'
  write ( *, '(a)' ) '  R8ST_WRITE writes an ST file.'

  call i4vec_dec ( nst, ist )
  call i4vec_dec ( nst, jst )

  i_min = i4vec_min ( nst, ist )
  i_max = i4vec_max ( nst, ist )
  j_min = i4vec_min ( nst, jst )
  j_max = i4vec_max ( nst, jst )

  call r8st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )

  call r8st_print ( m, n, nst, ist, jst, ast, '  Sparse Triplet (ST) data:' )

  call r8st_write ( output_filename, m, n, nst, ist, jst, ast )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Wrote the matrix data to "' &
    // trim ( output_filename ) // '".'

  return
end
subroutine r8st_read_test ( )

!*****************************************************************************80
!
!! R8ST_READ_TEST tests R8ST_HEADER_READ, R8ST_DATA_READ.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable, dimension ( : ) :: ast
  integer ( kind = 4 ) i_max
  integer ( kind = 4 ) i_min
  character ( len = 255 ) :: input_filename = 'kershaw_r8.st'
  integer ( kind = 4 ), allocatable, dimension ( : ) :: ist
  integer ( kind = 4 ) j_max
  integer ( kind = 4 ) j_min
  integer ( kind = 4 ), allocatable, dimension ( : ) :: jst
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nst

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ST_READ_TEST'
  write ( *, '(a)' ) '  R8ST_HEADER_READ reads the header from an ST file.'
  write ( *, '(a)' ) '  R8ST_DATA_READ reads the data from an ST file.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Read the data from "' // trim ( input_filename ) // '".'

  call r8st_header_read ( input_filename, i_min, i_max, j_min, j_max, m, n, nst )

  call r8st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )

  allocate ( ast(1:nst) )
  allocate ( ist(1:nst) )
  allocate ( jst(1:nst) )

  call r8st_data_read ( input_filename, m, n, nst, ist, jst, ast )

  call r8st_print ( m, n, nst, ist, jst, ast, &
    '  Sparse Triplet (ST) data read from file:' )

  deallocate ( ast )
  deallocate ( ist )
  deallocate ( jst )

  return
end
subroutine r8st_sort_a_test ( )

!*****************************************************************************80
!
!! R8ST_SORT_A_TEST tests R8ST_SORT_A.
!
!  Discussion:
!
!    The matrix is:
!
!      11  12   0   0  15
!      21  22   0   0   0
!       0   0  33   0  35
!       0   0   0  44   0
!      51   0  53   0  55
!
!    The index vectors are 1 based, and so have to be converted to
!    0-base before being written.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: nst = 11

  real ( kind = 8 ), dimension ( nst ) :: ast = (/ &
    51.0D+00, 12.0D+00, 11.0D+00, 33.0D+00, 15.0D+00, &
    53.0D+00, 55.0D+00, 22.0D+00, 35.0D+00, 44.0D+00, &
    21.0D+00 /)
  integer ( kind = 4 ) i_max
  integer ( kind = 4 ) i_min
  integer ( kind = 4 ) i4vec_max
  integer ( kind = 4 ) i4vec_min
  integer ( kind = 4 ), dimension ( nst ) :: ist = (/ &
     5, 1, 1, 3, 1, 5, 5, 2, 3, 4, 2 /)
  integer ( kind = 4 ) j_max
  integer ( kind = 4 ) j_min
  integer ( kind = 4 ), dimension ( nst ) :: jst= (/ &
     1, 2, 1, 3, 5, 3, 5, 2, 5, 4, 1 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8ST_SORT_A'
  write ( *, '(a)' ) '  R8ST_SORT_A sorts an ST matrix by columns.'

  i_min = i4vec_min ( nst, ist )
  i_max = i4vec_max ( nst, ist )
  j_min = i4vec_min ( nst, jst )
  j_max = i4vec_max ( nst, jst )

  call r8st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )

  call r8st_print ( m, n, nst, ist, jst, ast, &
    '  Matrix data before sorting:' )

  call r8st_sort_a ( m, n, nst, ist, jst, ast )

  call r8st_print ( m, n, nst, ist, jst, ast, &
    '  Matrix data sorted by column:' )

  return
end
