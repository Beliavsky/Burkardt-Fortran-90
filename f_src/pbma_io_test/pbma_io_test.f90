program main

!*****************************************************************************80
!
!! MAIN is the main program for PBMA_IO_TEST.
!
!  Discussion:
!
!    PBMA_IO_TEST tests the PBMA_IO library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PBMA_IO_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the PBMA_IO library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'PBMA_IO_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests PBMA_EXAMPLE and PBMA_WRITE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: ncol = 300
  integer ( kind = 4 ), parameter :: nrow = 300

  integer ( kind = 4 ) b(nrow,ncol)
  character ( len = 80 ) :: file_name = 'pbma_io_test_01.ascii.pbm'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  PBMA_EXAMPLE sets up ASCII PBM data.'
  write ( *, '(a)' ) '  PBMA_WRITE writes an ASCII PBM file.'

  call pbma_example ( nrow, ncol, b )

  call pbma_write ( file_name, nrow, ncol, b )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Wrote the header and data for "' &
    // trim ( file_name ) //'".'
  write ( *, '(a,i8)' ) '  Number of rows of data =    ', nrow
  write ( *, '(a,i8)' ) '  Number of columns of data = ', ncol

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests PBMA_READ.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable, dimension ( :, : ) :: b
  character ( len = 80 ) :: file_name = 'pbma_io_test_02.ascii.pbm'
  integer ( kind = 4 ) file_unit
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) ncol
  integer ( kind = 4 ) nrow

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  PBMA_READ reads an ASCII PBM file.'

  call pbma_write_test ( file_name )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  PBMA_WRITE_TEST created some data.'

  call get_unit ( file_unit )

  open ( unit = file_unit, file = file_name, status = 'old', iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST02 - Fatal error!'
    write ( *, '(a)' ) '  Could not open the file.'
    return
  end if

  call pbma_read_header ( file_unit, nrow, ncol )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  PBMA_READ_HEADER has read the header.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of rows of data =    ', nrow
  write ( *, '(a,i8)' ) '  Number of columns of data = ', ncol

  allocate ( b(nrow,ncol) )

  call pbma_read_data ( file_unit, nrow, ncol, b )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  PBMA_READ_DATA has read the data.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Sample data:'
  write ( *, '(a)' ) ' '
  do k = 1, 30
    i = ( ( 30 - k ) * 1 + ( k - 1 ) * nrow ) / ( 30 - 1 )
    j = ( ( 30 - k ) * 1 + ( k - 1 ) * ncol ) / ( 30 - 1 )
    write ( *, '(2x,i8,2x,i8,2x,i8)' ) i, j, b(i,j)
  end do

  call pbma_check_data ( nrow, ncol, b )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The data was accepted by PBMA_CHECK_DATA.'

  deallocate ( b )

  return
end

