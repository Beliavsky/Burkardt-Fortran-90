program main

!*****************************************************************************80
!
!! MAIN is the main program for ccs_io_test.
!
!  Discussion:
!
!    ccs_io_test tests the ccs_io library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ccs_io_test'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the ccs_io library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ccs_io_test'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests ccs_WRITE using a tiny matrix.
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
!    The 1-based CCS representation is
!
!      #  ICC  CCC  ACC
!     --  ---  ---  ---
!      1    1    1    2
!      2    2         3
!
!      3    1    3    3
!      4    3        -1
!      5    5         4
!
!      6    2    6    4
!      7    3        -3
!      8    4         1
!      9    5         2
!
!     10    3   10    2
!
!     11    2   11    6
!     12    5         1
!
!     13    *   13
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ), parameter :: ncc = 12

  real ( kind = 8 ), dimension ( ncc ) :: acc = (/ &
    2.0,  3.0, &
    3.0, -1.0,  4.0, &
    4.0, -3.0,  1.0, 2.0, &
    2.0, &
    6.0, 1.0 /)
  integer ( kind = 4 ), dimension ( n + 1 ) :: ccc = (/ &
    1, 3, 6, 10, 11, 13 /)
  integer ( kind = 4 ), dimension ( ncc ) :: icc = (/ &
    1, 2, &
    1, 3, 5, &
    2, 3, 4, 5, &
    3, &
    2, 5 /)
  character ( len = 255 ) :: prefix = 'simple'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Write a sparse matrix in CCS format to 3 files.'
!
!  Full storage statistics
!
  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Full rows    M = ', m
  write ( *, '(a,i4)' ) '  Full columns N = ', n
  write ( *, '(a,i4)' ) '  Full storage   = ', m * n
!
!  Print the CCS matrix.
!
  call ccs_print ( m, n, ncc, icc, ccc, acc, '  The matrix in 1-based CCS format:' )
!
!  Write the matrix to 3 files.
!
  call ccs_write ( prefix, ncc, n, icc, ccc, acc )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests ccs_HEADER_READ and ccs_DATA_READ.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: acc(:)
  integer ( kind = 4 ), allocatable :: ccc(:)
  integer ( kind = 4 ), allocatable :: icc(:)
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncc
  character ( len = 255 ) :: prefix = 'simple'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Read a sparse matrix in CCS format from 3 files.'
!
!  Read the header.
!
  call ccs_header_read ( prefix, ncc, n )
!
!  Allocate space.
!
  allocate ( acc(1:ncc) )
  allocate ( ccc(1:n+1) )
  allocate ( icc(1:ncc) )
!
!  Read the matrix data.
!
  call ccs_data_read ( prefix, ncc, n, icc, ccc, acc )
!
!  Print the CCS matrix.
!
  m = n
  call ccs_print ( m, n, ncc, icc, ccc, acc, '  The matrix in 1-based CCS format:' )
!
!  Free memory.
!
  deallocate ( acc )
  deallocate ( ccc )
  deallocate ( icc )

  return
end
