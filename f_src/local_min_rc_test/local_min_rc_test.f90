program main

!*****************************************************************************80
!
!! MAIN is the main program for LOCAL_MIN_RC_TEST.
!
!  Discussion:
!
!    LOCAL_MIN_RC_TEST tests the LOCAL_MIN_RC library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ), external :: g_01
  real ( kind = 8 ), external :: g_02
  real ( kind = 8 ), external :: g_03
  real ( kind = 8 ), external :: g_04
  real ( kind = 8 ), external :: g_05
  real ( kind = 8 ), external :: g_06
  real ( kind = 8 ), external :: g_07
  real ( kind = 8 ) t

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LOCAL_MIN_RC_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test LOCAL_MIN_RC, the reverse communication version of '
  write ( *, '(a)' ) '  the Brent routine which seeks a local minimizer of a function F(X)'
  write ( *, '(a)' ) '  in an interval [A,B].'

  t = 10.0D+00 * sqrt ( epsilon ( t ) )

  a = 0.0D+00
  b = 3.141592653589793D+00
  call local_min_rc_test ( a, b, t, g_01, &
    'g_01(x) = ( x - 2 ) * ( x - 2 ) + 1' )

  a = 0.0D+00
  b = 1.0D+00
  call local_min_rc_test ( a, b, t, g_02, &
    'g_02(x) = x * x + exp ( - x )' )

  a = -2.0D+00
  b =  2.0D+00
  call local_min_rc_test ( a, b, t, g_03, &
    'g_03(x) = x^4 + 2x^2 + x + 3' )

  a =  0.0001D+00
  b =  1.0D+00
  call local_min_rc_test ( a, b, t, g_04, &
    'g_04(x) = exp ( x ) + 1 / ( 100 x )' )

  a =  0.0002D+00
  b = 2.0D+00
  call local_min_rc_test ( a, b, t, g_05, &
    'g_05(x) = exp ( x ) - 2x + 1/(100x) - 1/(1000000x^2)' )

  a = 1.8D+00
  b = 1.9D+00
  call local_min_rc_test ( a, b, t, g_06, &
    'g_06(x) = - x * sin ( 10 pi x ) - 1' )

  a = 0.0D+00
  b = 2.0D+00
  call local_min_rc_test ( a, b, t, g_07, &
    'g_07(x) = 2x^4 - 4x^2 + x + 20' )

  a = -2.0D+00
  b = 0.0D+00
  call local_min_rc_test ( a, b, t, g_07, &
    'g_07(x) = 2x^4 - 4x^2 + x + 20' )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LOCAL_MIN_RC_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine local_min_rc_test ( a, b, t, f, title )

!*****************************************************************************80
!
!! LOCAL_MIN_RC_TEST tests LOCAL_MIN_RC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 April 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the endpoints of the interval.
!
!    Input, real ( kind = 8 ) T, a positive absolute error tolerance.
!
!    Input, external real ( kind = 8 ) F, the name of a user-supplied
!    function, of the form "FUNCTION F ( X )", which evaluates the
!    function whose local minimum is being sought.
!
!    Input, character ( LEN = * ) TITLE, a title for the problem.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ) arg
  real ( kind = 8 ) b
  real ( kind = 8 ) b2
  real ( kind = 8 ), external :: f
  integer ( kind = 4 ) status
  integer ( kind = 4 ) step
  real ( kind = 8 ) t
  character ( len = * ) title
  real ( kind = 8 ) value

  write ( *, '(a)' ) ' '
  write ( *, '(2x,a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Step      X                          F(X)'
  write ( *, '(a)' ) ' '
  step = 0

  arg = a
  value = f ( arg )
  write ( *, '(2x,i4,2x,g24.16,2x,g24.16)' ) step, arg, value

  arg = b
  value = f ( arg )
  write ( *, '(2x,i4,2x,g24.16,2x,g24.16)' ) step, arg, value

  a2 = a
  b2 = b
  status = 0

  do

    call local_min_rc ( a2, b2, arg, status, value )
 
    if ( status < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LOCAL_MIN_RC_TEST7 - Fatal error!'
      write ( *, '(a)' ) '  LOCAL_MIN_RC returned negative status.'
      exit
    end if

    value = f ( arg )

    step = step + 1
    write ( *, '(2x,i4,2x,g24.16,2x,g24.16)' ) step, arg, value

    if ( status == 0 ) then
      exit
    end if 

  end do

  return
end
function g_01 ( x )

!*****************************************************************************80
!
!! G_01 evaluates (x-2)^2 + 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 April 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which F is to be evaluated.
!
!    Output, real ( kind = 8 ) G_01, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) g_01
  real ( kind = 8 ) x

  g_01 = ( x - 2.0D+00 ) * ( x - 2.0D+00 ) + 1.0D+00

  return
end
function g_02 ( x )

!*****************************************************************************80
!
!! G_02 evaluates x^2 + exp ( - x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 April 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which F is to be evaluated.
!
!    Output, real ( kind = 8 ) G_02, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) g_02
  real ( kind = 8 ) x

  g_02 = x * x + exp ( - x )

  return
end
function g_03 ( x )

!*****************************************************************************80
!
!! G_03 evaluates x^4+2x^2+x+3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 April 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which F is to be evaluated.
!
!    Output, real ( kind = 8 ) G_03, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) g_03
  real ( kind = 8 ) x

  g_03 = ( ( x * x + 2.0D+00 ) * x + 1.0D+00 ) * x + 3.0D+00

  return
end
function g_04 ( x )

!*****************************************************************************80
!
!! G_04 evaluates exp(x)+1/(100X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 April 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which F is to be evaluated.
!
!    Output, real ( kind = 8 ) G_04, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) g_04
  real ( kind = 8 ) x

  g_04 = exp ( x ) + 0.01D+00 / x

  return
end
function g_05 ( x )

!*****************************************************************************80
!
!! G_05 evaluates exp(x) - 2x + 1/(100x) - 1/(1000000x^2)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 April 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which F is to be evaluated.
!
!    Output, real ( kind = 8 ) G_05, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) g_05
  real ( kind = 8 ) x

  g_05 = exp ( x ) - 2.0D+00 * x + 0.01D+00 / x - 0.000001D+00 / x / x

  return
end
function g_06 ( x )

!*****************************************************************************80
!
!! G_06 evaluates - x * sin ( 10 pi x ) - 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 November 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which F is to be evaluated.
!
!    Output, real ( kind = 8 ) G_06, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) g_06
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  g_06 = - x * sin ( 10.0 * r8_pi * x ) - 1.0D+00

  return
end
function g_07 ( x )

!*****************************************************************************80
!
!! G_07 evaluates 2x^4 - 4x^2 + x + 20
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 September 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the point at which F is to be evaluated.
!
!    Output, real ( kind = 8 ) G_07, the value of the function at X.
!
  implicit none

  real ( kind = 8 ) g_07
  real ( kind = 8 ) x

  g_07 = 2.0D+00 * x**4 - 4.0D+00 * x**2 + x + 20.0D+00

  return
end

