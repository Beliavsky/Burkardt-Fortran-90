subroutine bisection_rc ( a, b, x, fx, job )

!*****************************************************************************80
!
!! BISECTION_RC seeks a zero of f(x) in a change of sign interval.
!
!  Discussion:
!
!    The bisection method is used.
!
!    This routine uses reverse communication, so that the function is always
!    evaluated in the calling program.
!
!    On the first call, the user sets JOB = 0, and the values of A and B.
!    Thereafter, the user checks the returned value of JOB and follows 
!    directions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real ( kind = 8 ) A, B, the endpoints of the change of 
!    sign interval.  These values will change from call to call as the
!    interval size is reduced.
!
!    Output, real ( kind = 8 ) X, a point at which the function is to 
!    be evaluated.
!
!    Input, real ( kind = 8 ) FX, the function value at X.
!
!    Input/output, integer ( kind = 4 ) JOB, a communication flag.
!    The user sets JOB to 0 before the first call.  Thereafter, the program
!    controls setting the value of JOB, whose output values mean:
!    
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ), save :: fa
  real ( kind = 8 ), save :: fb
  real ( kind = 8 ) fx
  integer ( kind = 4 ) job
  real ( kind = 8 ) r8_sign
  integer ( kind = 4 ), save :: state
  real ( kind = 8 ) x

  if ( job == 0 ) then

    fa = 0.0D+00
    fb = 0.0D+00
    state = 1
    x = a
    job = 1

  else if ( state == 1 ) then

    fa = fx
    x = b
    state = 2

  else if ( state == 2 ) then

    fb = fx

    if ( r8_sign ( fa ) == r8_sign ( fb ) ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BISECTION_RC - Fatal error!'
      write ( *, '(a)' ) '  F(A) and F(B) have the same sign.'
      stop
    end if

    x = ( a + b ) / 2.0D+00
    state = 3

  else

    if ( r8_sign ( fx ) == r8_sign ( fa ) ) then
      a = x
      fa = fx
    else
      b = x
      fb = fx
    end if
    x = ( a + b ) / 2.0D+00
    state = 3

  end if

  return
end
function r8_sign ( x )

!*****************************************************************************80
!
!! R8_SIGN returns the sign of an R8.
!
!  Discussion:
!
!    value = -1 if X < 0;
!    value = +1 if X => 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the number whose sign is desired.
!
!    Output, real ( kind = 8 ) R8_SIGN, the sign of X:
!
  implicit none

  real ( kind = 8 ) r8_sign
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    r8_sign = -1.0D+00
  else
    r8_sign = +1.0D+00
  end if

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
