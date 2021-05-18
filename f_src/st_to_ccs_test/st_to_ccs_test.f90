program main

!*****************************************************************************80
!
!! MAIN is the main program for ST_TO_ccs_TEST.
!
!  Discussion:
!
!    ST_TO_ccs_TEST tests ST_TO_ccs.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ST_TO_ccs_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test st_to_ccs.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'st_to_ccs_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests st_to_ccs using a tiny matrix.
!
!  Discussion:
!
!    This test uses a trivial matrix whose full representation is:
!
!          2  3  0  0  0
!          3  0  4  0  6
!      A = 0 -1 -3  2  0
!          0  0  1  0  0
!          0  4  2  0  1
!
!    A (1-based) ST representation, reading in order by rows is:
!
!      I  J   A
!     -- --  --
!      1  1   2
!      1  2   3
!
!      2  1   3
!      2  3   4
!      2  5   6
!
!      3  2  -1
!      3  3  -3
!      3  4   2
!
!      4  3   1
!
!      5  2   4
!      5  3   2
!      5  5   1
!
!    The CCS representation (which goes in order by columns) is
!
!      #   I  JC   A
!     --  --  --  --
!      1   1   1   2
!      2   2       3
!
!      3   1   3   3
!      4   3      -1
!      5   5       4
!
!      6   2   6   4
!      7   3      -3
!      8   4       1
!      9   5       2
!
!     10   3  10   2
!
!     11   2  11   6
!     12   5       1
!
!     13   *  13
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nst = 12

  real ( kind = 8 ), allocatable :: acc(:)
  real ( kind = 8 ), dimension ( nst ) :: ast = (/ &
    2.0,  3.0, &
    3.0,  4.0,  6.0, &
   -1.0, -3.0,  2.0, &
    1.0, &
    4.0,  2.0,  1.0 /)
  integer ( kind = 4 ), allocatable :: ccc(:)
  integer ( kind = 4 ) i_max
  integer ( kind = 4 ) i_min
  integer ( kind = 4 ) i4vec_max
  integer ( kind = 4 ) i4vec_min
  integer ( kind = 4 ), allocatable :: icc(:)
  integer ( kind = 4 ), dimension ( nst ) :: ist = (/ &
    1, 1, &
    2, 2, 2, &
    3, 3, 3, &
    4, &
    5, 5, 5 /)
  integer ( kind = 4 ) j_max
  integer ( kind = 4 ) j_min
  integer ( kind = 4 ), dimension ( nst ) :: jst = (/ &
    1, 2, &
    1, 3, 5, &
    2, 3, 4, &
    3, &
    2, 3, 5 /)
  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ) ncc

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Convert a sparse matrix from ST to CCS format.'
  write ( *, '(a)' ) '  ST:  sparse triplet,    I, J,  A.'
  write ( *, '(a)' ) '  CCS: compressed column, I, CC, A.'

  i_min = i4vec_min ( nst, ist )
  i_max = i4vec_max ( nst, ist )
  j_min = i4vec_min ( nst, jst )
  j_max = i4vec_max ( nst, jst )

  call st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )
!
!  Print the ST matrix.
!
  call st_print ( m, n, nst, ist, jst, ast, '  The matrix in ST format:' )
!
!  Get the CCS size.
!
  call st_to_ccs_size ( nst, ist, jst, ncc )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of CCS values = ', ncc
!
!  Create the CCS indices.
!
  allocate ( icc(1:ncc) )
  allocate ( ccc(1:n+1) )

  call st_to_ccs_index ( nst, ist, jst, ncc, n, icc, ccc )
!
!  Create the CCS values.
!
  allocate ( acc(1:ncc) )

  call st_to_ccs_values ( nst, ist, jst, ast, ncc, n, icc, ccc, acc )
!
!  Print the CCS matrix.
!
  call ccs_print ( m, n, ncc, icc, ccc, acc, '  CCS Matrix:' )
!
!  Free memory.
!
  deallocate ( acc )
  deallocate ( ccc )
  deallocate ( icc )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests st_to_ccs on a matrix stored in a file.
!
!  Discussion:
!
!    We assume no prior knowledge about the matrix except the filename.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: acc(:)
  real ( kind = 8 ), allocatable :: ast(:)
  integer ( kind = 4 ), allocatable :: ccc(:)
  character ( len = 255 ) filename_st
  integer ( kind = 4 ) i_max
  integer ( kind = 4 ) i_min
  integer ( kind = 4 ), allocatable :: icc(:)
  integer ( kind = 4 ), allocatable :: ist(:)
  integer ( kind = 4 ) j_max
  integer ( kind = 4 ) j_min
  integer ( kind = 4 ), allocatable :: jst(:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncc
  integer ( kind = 4 ) nst

  filename_st = 'west_st.txt'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Convert a sparse matrix from ST to CCS format.'
  write ( *, '(a)' ) '  ST:  sparse triplet,    I, J,  A.'
  write ( *, '(a)' ) '  CCS: compressed column, I, CC, A.'
  write ( *, '(a)' ) '  This matrix is read from the file "' &
    // trim ( filename_st ) // '".'
!
!  Get the size of the ST matrix.
!
  call st_header_read ( filename_st, i_min, i_max, j_min, j_max, m, n, nst )

  call st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )
!
!  Allocate space.
!
  allocate ( ist(nst) )
  allocate ( jst(nst) )
  allocate ( ast(nst) )
!
!  Read the ST matrix.
!
  call st_data_read ( filename_st, m, n, nst, ist, jst, ast )
!
!  Print the ST matrix.
!
  call st_print ( m, n, nst, ist, jst, ast, '  The matrix in ST format:' )
!
!  Get the CCS size.
!
  call st_to_ccs_size ( nst, ist, jst, ncc )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of CCS values = ', ncc
!
!  Create the CCS indices.
!
  allocate ( icc(1:ncc) )
  allocate ( ccc(1:n+1) )

  call st_to_ccs_index ( nst, ist, jst, ncc, n, icc, ccc )
!
!  Create the CCS values.
!
  allocate ( acc(1:ncc) )

  call st_to_ccs_values ( nst, ist, jst, ast, ncc, n, icc, ccc, acc )
!
!  Print the CCS matrix.
!
  call ccs_print ( m, n, ncc, icc, ccc, acc, '  CCS Matrix:' )
!
!  Free memory.
!
  deallocate ( acc )
  deallocate ( ast )
  deallocate ( ccc )
  deallocate ( icc )
  deallocate ( ist )
  deallocate ( jst )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 creates a CCS sparse matrix file from an ST file.
!
!  Discussion:
!
!    We assume no prior knowledge about the matrix except the filename.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: acc(:)
  real ( kind = 8 ), allocatable :: ast(:)
  integer ( kind = 4 ), allocatable :: ccc(:)
  character ( len = 255 ) filename_acc
  character ( len = 255 ) filename_ccc
  character ( len = 255 ) filename_icc
  character ( len = 255 ) filename_st
  integer ( kind = 4 ) i_max
  integer ( kind = 4 ) i_min
  integer ( kind = 4 ), allocatable :: icc(:)
  integer ( kind = 4 ), allocatable :: ist(:)
  integer ( kind = 4 ) j_max
  integer ( kind = 4 ) j_min
  integer ( kind = 4 ), allocatable :: jst(:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncc
  integer ( kind = 4 ) nst

  filename_st = 'west_st.txt'
  filename_acc = 'west_acc.txt'
  filename_ccc = 'west_ccc.txt'
  filename_icc = 'west_icc.txt'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Convert a sparse matrix from ST to CCS format.'
  write ( *, '(a)' ) '  ST:  sparse triplet,    I, J,  A.'
  write ( *, '(a)' ) '  CCS: compressed column, I, CC, A.'
  write ( *, '(a)' ) '  The ST matrix is read from the file "' &
    // trim ( filename_st ) // '",'
  write ( *, '(a)' ) '  and the CCS matrix is written to the files:'
  write ( *, '(a)' ) '    "'// trim ( filename_icc ) // '",'
  write ( *, '(a)' ) '    "'// trim ( filename_ccc ) // '", and'
  write ( *, '(a)' ) '    "'// trim ( filename_acc ) // '",'
!
!  Get the size of the ST matrix.
!
  call st_header_read ( filename_st, i_min, i_max, j_min, j_max, m, n, nst )

  call st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )
!
!  Allocate space.
!
  allocate ( ist(nst) )
  allocate ( jst(nst) )
  allocate ( ast(nst) )
!
!  Read the ST matrix.
!
  call st_data_read ( filename_st, m, n, nst, ist, jst, ast )
!
!  Get the CCS size.
!
  call st_to_ccs_size ( nst, ist, jst, ncc )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of CCS values = ', ncc
!
!  Create the CCS indices.
!
  allocate ( icc(1:ncc) )
  allocate ( ccc(1:n+1) )

  call st_to_ccs_index ( nst, ist, jst, ncc, n, icc, ccc )
!
!  Create the CCS values.
!
  allocate ( acc(1:ncc) )

  call st_to_ccs_values ( nst, ist, jst, ast, ncc, n, icc, ccc, acc )
!
!  Write the CCS matrix.
!
  call i4vec_write ( filename_icc, ncc, icc )
  call i4vec_write ( filename_ccc, n + 1, ccc )
  call r8vec_write ( filename_acc, ncc, acc )
!
!  Free memory.
!
  deallocate ( acc )
  deallocate ( ast )
  deallocate ( ccc )
  deallocate ( icc )
  deallocate ( ist )
  deallocate ( jst )

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 works with a CCS sparse matrix with many repeated index pairs.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: acc(:)
  real ( kind = 8 ), allocatable :: ast(:)
  real ( kind = 8 ), allocatable :: b1(:)
  real ( kind = 8 ), allocatable :: b2(:)
  integer ( kind = 4 ), allocatable :: ccc(:)
  integer ( kind = 4 ) i_max
  integer ( kind = 4 ) i_min
  integer ( kind = 4 ) i4vec_max
  integer ( kind = 4 ) i4vec_min
  integer ( kind = 4 ), allocatable :: icc(:)
  integer ( kind = 4 ), allocatable :: ist(:)
  integer ( kind = 4 ) j_max
  integer ( kind = 4 ) j_min
  integer ( kind = 4 ), allocatable :: jst(:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncc
  integer ( kind = 4 ) nst
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real ( kind = 8 ) r
  real ( kind = 8 ) r8vec_diff_norm
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  Convert a sparse matrix from ST to CCS format.'
  write ( *, '(a)' ) '  ST:  sparse triplet,    I, J,  A.'
  write ( *, '(a)' ) '  CCS: compressed column, I, CC, A.'
  write ( *, '(a)' ) '  The ST matrix is the Wathen finite element matrix.'
  write ( *, '(a)' ) '  It has many repeated index pairs.'
  write ( *, '(a)' ) '  To check, compare ACC*X - AST*X for a random X.'
!
!  Get the size of the ST matrix.
!
  nx = 3
  ny = 3
  call wathen_st_size ( nx, ny, nst )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of ST values = ', nst
!
!  Set the formal matrix size
!
  m = 3 * nx * ny + 2 * nx + 2 * ny + 1
  n = m
!
!  Set a random vector.
!
  seed = 123456789
  allocate ( x(1:n) )
  call r8vec_uniform_01 ( n, seed, x )
!
!  Allocate space.
!
  allocate ( ist(nst) )
  allocate ( jst(nst) )
  allocate ( ast(nst) )
!
!  Create the ST matrix.
!
  seed = 123456789
  call wathen_st ( nx, ny, nst, seed, ist, jst, ast )

  i_min = i4vec_min ( nst, ist )
  i_max = i4vec_max ( nst, ist )
  j_min = i4vec_min ( nst, jst )
  j_max = i4vec_max ( nst, jst )

  call st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )
!
!  Compute B1 = AST * X
!
  allocate ( b1(1:n) )
  call st_mv ( m, n, nst, ist, jst, ast, x, b1 )
!
!  Get the CCS size.
!
  call st_to_ccs_size ( nst, ist, jst, ncc )

  write ( *, '(a,i4)' ) '  Number of CCS values = ', ncc
!
!  Create the CCS indices.
!
  allocate ( icc(1:ncc) )
  allocate ( ccc(1:n+1) )
  call st_to_ccs_index ( nst, ist, jst, ncc, n, icc, ccc )
!
!  Create the CCS values.
!
  allocate ( acc(1:ncc) )
  call st_to_ccs_values ( nst, ist, jst, ast, ncc, n, icc, ccc, acc )
!
!  Compute B2 = ACC * X.
!
  allocate ( b2(1:n) )
  call ccs_mv ( m, n, ncc, icc, ccc, acc, x, b2 )
!
!  Compare B1 and B2.
!
  r = r8vec_diff_norm ( n, b1, b2 )
  write ( *, '(a,g14.6)' ) '  || ACC*X - AST*X|| = ', r
!
!  Free memory.
!
  deallocate ( acc )
  deallocate ( ast )
  deallocate ( b1 )
  deallocate ( b2 )
  deallocate ( ccc )
  deallocate ( icc )
  deallocate ( ist )
  deallocate ( jst )
  deallocate ( x )

  return
end
