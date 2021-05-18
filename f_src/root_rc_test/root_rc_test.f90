program main

!*****************************************************************************80
!
!! MAIN is the main program for ROOT_RC_TEST.
!
!  Discussion:
!
!    ROOT_RC_TEST tests the ROOT_RC library.
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

  real ( kind = 8 ) ferr
  real ( kind = 8 ) fx
  integer ( kind = 4 ) it
  integer ( kind = 4 ) it_max
  real ( kind = 8 ) q(9)
  real ( kind = 8 ) root_rc
  real ( kind = 8 ) x
  real ( kind = 8 ) xerr

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ROOT_RC_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the ROOT_RC library.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '       X              XERR            FX              FERR'
  write ( *, '(a)' ) ' '
!
!  Initialization.
!
  it = 0
  it_max = 30
  q(1:9) = 0.0D+00
  x = - 2.1D+00
!
!  Each call takes one more step of improvement.
!
  do

    fx = cos ( x ) - x

    if ( it == 0 ) then 
      write ( *, '(2x,g14.6,2x,14x,2x,g14.6)' ) x, fx
    else
      write ( *, '(4(2x,g14.6))' ) x, xerr, fx, ferr
    end if

    x = root_rc ( x, fx, ferr, xerr, q )

    if ( ferr < 1.0D-08 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Uncertainty in F(X) less than tolerance'
      exit
    end if

    if ( xerr < 1.0D-08 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Width of X interal less than tolerance.'
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
  write ( *, '(a)' ) 'ROOT_RC_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end

