program main 

!*****************************************************************************80
!
!! MAIN is the main program for LINE_GRID_TEST.
!
!  Discussion:
!
!    LINE_GRID_TEST tests LINE_GRID.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LINE_GRID_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LINE_GRID library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LINE_GRID_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests LINE_GRID using simple parameters.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11

  real ( kind = 8 ), parameter :: a = -1.0D+00
  real ( kind = 8 ), parameter :: b = +1.0D+00
  integer ( kind = 4 ), parameter :: c = 1
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Create a grid using LINE_GRID.'
  write ( *, '(a)' ) '  Use simple parameters.'
  write ( *, '(a,i4)' ) '  Number of grid points N = ', n
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     N      C      A         B'
  write ( *, '(a)' ) ''
  write ( *, '(2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) n, c, a, b

  call line_grid ( n, a, b, c, x )
  call r8vec_print ( n, x, '  Grid points:' )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 changes the number of points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), parameter :: a = 0.0D+00
  real ( kind = 8 ), parameter :: b = 1.0D+00
  integer ( kind = 4 ), parameter :: c = 2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) test
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Create a grid using LINE_GRID.'
  write ( *, '(a)' ) '  Try an increasing number of points.'

  n = 4

  do test = 1, 3

    n = 2 * n + 1
    allocate ( x(1:n) )
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '     N      C      A         B'
    write ( *, '(a)' ) ''
    write ( *, '(2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) n, c, a, b

    call line_grid ( n, a, b, c, x )
    call r8vec_print ( n, x, '  Grid points:' )

    deallocate ( x )

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tries all the centering options.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 11

  real ( kind = 8 ), parameter :: a = 0.0D+00
  real ( kind = 8 ), parameter :: b = 100.0D+00
  integer ( kind = 4 ) c
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Try the different centering options.'
  write ( *, '(a,i4)' ) '  Number of grid points N = ', n

  do c = 1, 5

    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '     N      C      A         B'
    write ( *, '(a)' ) ''
    write ( *, '(2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) n, c, a, b

    call line_grid ( n, a, b, c, x )
    call r8vec_print ( n, x, '  Grid points:' )

  end do

  return
end

