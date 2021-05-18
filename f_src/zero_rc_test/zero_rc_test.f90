program main

!*****************************************************************************80
!
!! MAIN is the main program for ZERO_RC_TEST.
!
!  Discussion:
!
!    ZERO_RC_TEST tests the ZERO_RC library.
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

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ), external :: f_01
  real ( kind = 8 ), external :: f_02
  real ( kind = 8 ), external :: f_03
  real ( kind = 8 ), external :: f_04
  real ( kind = 8 ), external :: f_05
  real ( kind = 8 ) machep
  real ( kind = 8 ) t


  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ZERO_RC_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  ZERO_RC seeks a root of a function F(X)'
  write ( *, '(a)' ) '  in an interval [A,B], using reverse communication.'

  machep = epsilon ( machep )
  t = machep

  a = 1.0D+00
  b = 2.0D+00

  call example_test ( a, b, machep, t, f_01, &
    'f_01(x) = sin ( x ) - x / 2' )

  a = 0.0D+00
  b = 1.0D+00

  call example_test ( a, b, machep, t, f_02, &
    'f_02(x) = 2 * x - exp ( - x )' )

  a = -1.0D+00
  b =  0.5D+00

  call example_test ( a, b, machep, t, f_03, &
    'f_03(x) = x * exp ( - x )' )

  a =  0.0001D+00
  b =  20.0D+00

  call example_test ( a, b, machep, t, f_04, &
    'f_04(x) = exp ( x ) - 1 / ( 100 * x * x )' )

  a = -5.0D+00
  b =  2.0D+00

  call example_test ( a, b, machep, t, f_05, &
    'f_05(x) = (x+3) * (x-1) * (x-1)' )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'ZERO_RC_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine example_test ( a, b, machep, t, f, title )

!*****************************************************************************80
!
!! EXAMPLE_TEST tests Brent's zero finding routine on one test function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the two endpoints of the change of sign
!    interval.
!
!    Input, real ( kind = 8 ) MACHEP, an estimate for the relative machine
!    precision.
!
!    Input, real ( kind = 8 ) T, a positive error tolerance.
!
!    Input, external real ( kind = 8 ) F, the name of a user-supplied
!    function, of the form "FUNCTION F ( X )", which evaluates the
!    function whose zero is being sought.
!
!    Input, character ( LEN = * ) TITLE, a title for the problem.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) arg
  real ( kind = 8 ) b
  real ( kind = 8 ), external :: f
  real ( kind = 8 ) machep
  integer ( kind = 4 ) status
  real ( kind = 8 ) t
  character ( len = *  ) title
  real ( kind = 8 ) value
  
  write ( *, '(a)' ) ' '
  write ( *, '(2x,a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    STATUS      X               F(X)'
  write ( *, '(a)' ) ' '

  status = 0

  do 

    call zero_rc ( a, b, t, arg, status, value )

    if ( status < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ZERO_RC returned an error flag!'
      exit
    end if

    value = f ( arg )

    write ( *, '(2x,i8,2x,g14.8,2x,g14.8)' ) status, arg, value

    if ( status == 0 ) then
      exit 
    end if

  end do

  return
end
function f_01 ( x )

!*****************************************************************************80
!
!! F_01 evaluates sin ( x ) - x / 2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which F is to be evaluated.
!
!    Output, real ( kind = 8 ) F_01, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) f_01
  real ( kind = 8 ) x

  f_01 = sin ( x ) - 0.5D+00 * x

  return
end
function f_02 ( x )

!*****************************************************************************80
!
!! F_02 evaluates 2*x-exp(-x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which F is to be evaluated.
!
!    Output, real ( kind = 8 ) F_02, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) f_02
  real ( kind = 8 ) x

  f_02 = 2.0D+00 * x - exp ( - x )

  return
end
function f_03 ( x )

!*****************************************************************************80
!
!! F_03 evaluates x*exp(-x).
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which F is to be evaluated.
!
!    Output, real ( kind = 8 ) F_03, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) f_03
  real ( kind = 8 ) x

  f_03 = x * exp ( - x )

  return
end
function f_04 ( x )

!*****************************************************************************80
!
!! F_04 evaluates exp(x) - 1 / (100*x*x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which F is to be evaluated.
!
!    Output, real ( kind = 8 ) F_04, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) f_04
  real ( kind = 8 ) x

  f_04 = exp ( x ) - 1.0D+00 / 100.0D+00 / x / x

  return
end
function f_05 ( x )

!*****************************************************************************80
!
!! F_05 evaluates (x+3)*(x-1)*(x-1).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which F is to be evaluated.
!
!    Output, real ( kind = 8 ) F_05, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) f_05
  real ( kind = 8 ) x

  f_05 = ( x + 3.0D+00 ) * ( x - 1.0D+00 ) * ( x - 1.0D+00 )

  return
end

