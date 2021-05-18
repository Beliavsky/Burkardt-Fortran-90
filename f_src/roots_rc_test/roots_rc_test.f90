program main

!*****************************************************************************80
!
!! MAIN is the main program for ROOTS_RC_TEST.
!
!  Discussion:
!
!    ROOTS_RC_TEST tests the ROOTS_RC library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ) ferr
  real ( kind = 8 ) fx(n)
  integer ( kind = 4 ) it
  integer ( kind = 4 ), parameter :: it_max = 30
  real ( kind = 8 ) q(2*n+2,n+2)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xnew(n)

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ROOTS_RC_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the ROOTS_RC library.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       FERR          X'
  write ( *, '(a)' ) ' '
!
!  Initialization.
!
  q(1:2*n+2,1:n+2) = 0.0D+00

  xnew(1) = 1.2D+00
  xnew(2:n) = 1.0D+00

  it = 0

  do

    x(1:n) = xnew(1:n)

    fx(1) = 1.0D+00 - x(1)
    fx(2:n) = 10.0D+00 * ( x(2:n) - x(1:n-1)**2 )

    if ( it == 0 ) then
      write ( *, '(2x,14x,5(2x,g14.6))' ) x(1:n)
    else
      write ( *, '(2x,g14.6,5(2x,g14.6))' ) ferr, x(1:n)
    end if

    call roots_rc ( n, x, fx, ferr, xnew, q )

    if ( ferr < 1.0D-07 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Sum of |f(x)| less than tolerance.'
      exit
    end if

    if ( it_max < it ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Too many iterations!'
      exit
    end if

    it = it + 1

  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ROOTS_RC_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end

