subroutine filon_fun_cos ( n, f, a, b, t, value )

!*****************************************************************************80
!
!! FILON_FUN_COS uses Filon's method on integrals with a cosine factor.
!
!  Discussion:
!
!    The integral to be approximated has the form:
!
!      Integral ( A <= X <= B ) F(X) * COS(T*X) dX
!
!    where T is user specified.
!
!    The function is interpolated over each subinterval by
!    a parabolic arc.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Chase, Lloyd Fosdick,
!    An Algorithm for Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 453-457.
!
!    Stephen Chase, Lloyd Fosdick,
!    Algorithm 353:
!    Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 457-458.
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data points.
!    N must be odd, and greater than 1.
!
!    Input, external F, the subroutine which evaluates the integrand,
!    of the form subroutine F ( N, X, FX ).
!
!    Input, real ( kind = 8 ) A, B, the limits of integration.
!
!    Input, real ( kind = 8 ) T, the multiplier of the X argument of the cosine.
!
!    Output, real ( kind = 8 ) VALUE, the approximate value of the integral.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) c2n
  real ( kind = 8 ) c2nm1
  real ( kind = 8 ) cost
  external f
  real ( kind = 8 ) ftab(n)
  real ( kind = 8 ) gamma
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  real ( kind = 8 ) sint
  real ( kind = 8 ) t
  real ( kind = 8 ) theta
  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)

  if ( a == b ) then
    value = 0.0D+00
    return
  end if
 
  if ( n <= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILON_FUN_COS - Fatal error!'
    write ( *, '(a)' ) '  N < 2'
    write ( *, '(a,i8)' ) '  N = ', n
    stop 1
  end if
 
  if ( mod ( n, 2 ) /= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILON_FUN_COS - Fatal error!'
    write ( *, '(a)' ) '  N must be odd.'
    write ( *, '(a,i8)' ) '  N = ', n
    stop 1
  end if
!
!  Set the X values.
!
  do i = 1, n
    x(i) = ( real ( n - i,     kind = 8 ) * a   &
           + real (     i - 1, kind = 8 ) * b ) &
           / real ( n     - 1, kind = 8 )
  end do

  h = ( b - a ) / real ( n - 1, kind = 8 )
  theta = t * h
  sint = sin ( theta )
  cost = cos ( theta )

  if ( 6.0D+00 * abs ( theta ) <= 1.0D+00 ) then

    alpha = 2.0D+00 * theta**3 /   45.0D+00 &
          - 2.0D+00 * theta**5 /  315.0D+00 &
          + 2.0D+00 * theta**7 / 4725.0D+00
  
    beta =  2.0D+00            /     3.0D+00 &
          + 2.0D+00 * theta**2 /    15.0D+00 &
          - 4.0D+00 * theta**4 /   105.0D+00 &
          + 2.0D+00 * theta**6 /   567.0D+00 &
          - 4.0D+00 * theta**8 / 22275.0D+00

    gamma = 4.0D+00            /      3.0D+00 &
          - 2.0D+00 * theta**2 /     15.0D+00 &
          +           theta**4 /    210.0D+00 &
          -           theta**6 /  11340.0D+00

  else

    alpha = ( theta**2 + theta * sint * cost - 2.0D+00 * sint**2 ) / theta**3

    beta = ( 2.0D+00 * theta + 2.0D+00 * theta * cost**2 &
      - 4.0D+00 * sint * cost ) / theta**3

    gamma = 4.0D+00 * ( sint - theta * cost ) / theta**3
  
  end if
!
!  Tabulate the function.
!
  call f ( n, x, ftab )

  c2n = sum ( ftab(1:n:2) * cos ( t * x(1:n:2) ) ) &
    - 0.5D+00 * ( ftab(n) * cos ( t * x(n) ) &
                + ftab(1) * cos ( t * x(1) ) )

  c2nm1 = sum ( ftab(2:n-1:2) * cos ( t * x(2:n-1:2) ) )
 
  value = h * ( &
      alpha * ( ftab(n) * sin ( t * x(n) ) & 
              - ftab(1) * sin ( t * x(1) ) ) &
    + beta * c2n &
    + gamma * c2nm1 )

  return
end
subroutine filon_tab_cos ( n, ftab, a, b, t, value )

!*****************************************************************************80
!
!! FILON_TAB_COS uses Filon's method on integrals with a cosine factor.
!
!  Discussion:
!
!    The integral to be approximated has the form:
!
!      Integral ( A <= X <= B ) F(X) * COS(T*X) dX
!
!    where T is user specified.
!
!    The function is interpolated over each subinterval by
!    a parabolic arc.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Chase, Lloyd Fosdick,
!    An Algorithm for Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 453-457.
!
!    Stephen Chase, Lloyd Fosdick,
!    Algorithm 353:
!    Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 457-458.
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data points.
!    N must be odd, and greater than 1.
!
!    Input, real ( kind = 8 ) FTAB(N), contains the value of the function
!    at A, A+H, A+2*H, ... , B-H, B, where H = (B-A)/(N-1).
!
!    Input, real ( kind = 8 ) A, B, the limits of integration.
!
!    Input, real ( kind = 8 ) T, the multiplier of the X argument of the cosine.
!
!    Output, real ( kind = 8 ) VALUE, the approximate value of the integral.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) c2n
  real ( kind = 8 ) c2nm1
  real ( kind = 8 ) cost
  real ( kind = 8 ) ftab(n)
  real ( kind = 8 ) gamma
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  real ( kind = 8 ) sint
  real ( kind = 8 ) t
  real ( kind = 8 ) theta
  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)

  if ( a == b ) then
    value = 0.0D+00
    return
  end if
 
  if ( n <= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILON_TAB_COS - Fatal error!'
    write ( *, '(a)' ) '  N < 2'
    write ( *, '(a,i8)' ) '  N = ', n
    stop 1
  end if
 
  if ( mod ( n, 2 ) /= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILON_TAB_COS - Fatal error!'
    write ( *, '(a)' ) '  N must be odd.'
    write ( *, '(a,i8)' ) '  N = ', n
    stop 1
  end if
!
!  Set the X values.
!
  do i = 1, n
    x(i) = ( real ( n - i,     kind = 8 ) * a   &
           + real (     i - 1, kind = 8 ) * b ) &
           / real ( n     - 1, kind = 8 )
  end do

  h = ( b - a ) / real ( n - 1, kind = 8 )
  theta = t * h
  sint = sin ( theta )
  cost = cos ( theta )

  if ( 6.0D+00 * abs ( theta ) <= 1.0D+00 ) then

    alpha = 2.0D+00 * theta**3 /   45.0D+00 &
          - 2.0D+00 * theta**5 /  315.0D+00 &
          + 2.0D+00 * theta**7 / 4725.0D+00
  
    beta =  2.0D+00            /     3.0D+00 &
          + 2.0D+00 * theta**2 /    15.0D+00 &
          - 4.0D+00 * theta**4 /   105.0D+00 &
          + 2.0D+00 * theta**6 /   567.0D+00 &
          - 4.0D+00 * theta**8 / 22275.0D+00

    gamma = 4.0D+00            /      3.0D+00 &
          - 2.0D+00 * theta**2 /     15.0D+00 &
          +           theta**4 /    210.0D+00 &
          -           theta**6 /  11340.0D+00

  else

    alpha = ( theta**2 + theta * sint * cost - 2.0D+00 * sint**2 ) / theta**3

    beta = ( 2.0D+00 * theta + 2.0D+00 * theta * cost**2 &
      - 4.0D+00 * sint * cost ) / theta**3

    gamma = 4.0D+00 * ( sint - theta * cost ) / theta**3
  
  end if

  c2n = sum ( ftab(1:n:2) * cos ( t * x(1:n:2) ) ) &
    - 0.5D+00 * ( ftab(n) * cos ( t * x(n) ) &
                + ftab(1) * cos ( t * x(1) ) )

  c2nm1 = sum ( ftab(2:n-1:2) * cos ( t * x(2:n-1:2) ) )
 
  value = h * ( &
      alpha * ( ftab(n) * sin ( t * x(n) ) & 
              - ftab(1) * sin ( t * x(1) ) ) &
    + beta * c2n &
    + gamma * c2nm1 )

  return
end
subroutine filon_fun_sin ( n, f, a, b, t, value )

!*****************************************************************************80
!
!! FILON_FUN_SIN uses Filon's method on integrals with a sine factor.
!
!  Discussion:
!
!    The integral to be approximated has the form
!
!      Integral ( A <= X <= B ) F(X) * SIN(T*X) dX
!
!    where T is user specified.
!
!    The function is interpolated over each subinterval by
!    a parabolic arc.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Chase, Lloyd Fosdick,
!    An Algorithm for Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 453-457.
!
!    Stephen Chase, Lloyd Fosdick,
!    Algorithm 353:
!    Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 457-458.
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data points, 
!    including the endpoints.  N must be odd, and greater than 1.
!
!    Input, external F, the subroutine which evaluates the integrand,
!    of the form subroutine F ( N, X, FX ).
!
!    Input, real ( kind = 8 ) A, B, the limits of integration.
!
!    Input, real ( kind = 8 ) T, multiplier of the X argument of the sine.
!
!    Output, real ( kind = 8 ) VALUE, the approximate value of the integral.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) cost
  external f
  real ( kind = 8 ) ftab(n)
  real ( kind = 8 ) gamma
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  real ( kind = 8 ) s2n
  real ( kind = 8 ) s2nm1
  real ( kind = 8 ) sint
  real ( kind = 8 ) t
  real ( kind = 8 ) theta
  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)

  if ( a == b ) then
    value = 0.0D+00
    return
  end if
 
  if ( n <= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILON_FUN_SIN - Fatal error!'
    write ( *, '(a)' ) '  N < 2'
    write ( *, '(a,i8)' ) '  N = ', n
    stop 1
  end if
 
  if ( mod ( n, 2 ) /= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILON_FUN_SIN - Fatal error!'
    write ( *, '(a)' ) '  N must be odd.'
    write ( *, '(a,i8)' ) '  N = ', n
    stop 1
  end if
!
!  Set the X values.
!
  do i = 1, n
    x(i) = ( real ( n - i,     kind = 8 ) * a   &
           + real (     i - 1, kind = 8 ) * b ) &
           / real ( n     - 1, kind = 8 )
  end do

  h = ( b - a ) / real ( n - 1, kind = 8 )
  theta = t * h
  sint = sin ( theta )
  cost = cos ( theta )

  if ( 6.0D+00 * abs ( theta ) <= 1.0D+00 ) then

    alpha = 2.0D+00 * theta**3 /   45.0D+00 &
          - 2.0D+00 * theta**5 /  315.0D+00 &
          + 2.0D+00 * theta**7 / 4725.0D+00
  
    beta =  2.0D+00            /     3.0D+00 &
          + 2.0D+00 * theta**2 /    15.0D+00 &
          - 4.0D+00 * theta**4 /   105.0D+00 &
          + 2.0D+00 * theta**6 /   567.0D+00 &
          - 4.0D+00 * theta**8 / 22275.0D+00

    gamma = 4.0D+00            /      3.0D+00 &
          - 2.0D+00 * theta**2 /     15.0D+00 &
          +           theta**4 /    210.0D+00 &
          -           theta**6 /  11340.0D+00

  else
 
    alpha = ( theta**2 + theta * sint * cost &
      - 2.0D+00 * sint**2 ) / theta**3

    beta = ( 2.0D+00 * theta + 2.0D+00 * theta * cost**2 &
      - 4.0D+00 * sint * cost ) / theta**3

    gamma = 4.0D+00 * ( sint - theta * cost ) / theta**3
 
  end if
!
!  Tabulate the function.
!
  call f ( n, x, ftab )

  s2n = sum ( ftab(1:n:2) * sin ( t * x(1:n:2) ) ) &
    - 0.5D+00 * ( ftab(n) * sin ( t * x(n) ) &
                + ftab(1) * sin ( t * x(1) ) )

  s2nm1 = sum ( ftab(2:n-1:2) * sin ( t * x(2:n-1:2) ) )

  value = h * ( &
      alpha * ( ftab(1) * cos ( t * x(1) ) &
              - ftab(n) * cos ( t * x(n) ) ) &
    + beta * s2n &
    + gamma * s2nm1 )
 
  return
end
subroutine filon_tab_sin ( n, ftab, a, b, t, value )

!*****************************************************************************80
!
!! FILON_TAB_SIN uses Filon's method on integrals with a sine factor.
!
!  Discussion:
!
!    The integral to be approximated has the form
!
!      Integral ( A <= X <= B ) F(X) * SIN(T*X) dX
!
!    where T is user specified.
!
!    The function is interpolated over each subinterval by
!    a parabolic arc.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Chase, Lloyd Fosdick,
!    An Algorithm for Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 453-457.
!
!    Stephen Chase, Lloyd Fosdick,
!    Algorithm 353:
!    Filon Quadrature,
!    Communications of the Association for Computing Machinery,
!    Volume 12, Number 8, August 1969, pages 457-458.
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data points, 
!    including the endpoints.  N must be odd, and greater than 1.
!
!    Input, real ( kind = 8 ) FTAB(N), contains the value of the function
!    at A, A+H, A+2*H, ... , B-H, B, where H = (B-A)/(N-1).
!
!    Input, real ( kind = 8 ) A, B, the limits of integration.
!
!    Input, real ( kind = 8 ) T, multiplier of the X argument of the sine.
!
!    Output, real ( kind = 8 ) VALUE, the approximate value of the integral.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) cost
  real ( kind = 8 ) ftab(n)
  real ( kind = 8 ) gamma
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  real ( kind = 8 ) s2n
  real ( kind = 8 ) s2nm1
  real ( kind = 8 ) sint
  real ( kind = 8 ) t
  real ( kind = 8 ) theta
  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)

  if ( a == b ) then
    value = 0.0D+00
    return
  end if
 
  if ( n <= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILON_TAB_SIN - Fatal error!'
    write ( *, '(a)' ) '  N < 2'
    write ( *, '(a,i8)' ) '  N = ', n
    stop 1
  end if
 
  if ( mod ( n, 2 ) /= 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FILON_TAB_SIN - Fatal error!'
    write ( *, '(a)' ) '  N must be odd.'
    write ( *, '(a,i8)' ) '  N = ', n
    stop 1
  end if
!
!  Set the X values.
!
  do i = 1, n
    x(i) = ( real ( n - i,     kind = 8 ) * a   &
           + real (     i - 1, kind = 8 ) * b ) &
           / real ( n     - 1, kind = 8 )
  end do

  h = ( b - a ) / real ( n - 1, kind = 8 )
  theta = t * h
  sint = sin ( theta )
  cost = cos ( theta )

  if ( 6.0D+00 * abs ( theta ) <= 1.0D+00 ) then

    alpha = 2.0D+00 * theta**3 /   45.0D+00 &
          - 2.0D+00 * theta**5 /  315.0D+00 &
          + 2.0D+00 * theta**7 / 4725.0D+00
  
    beta =  2.0D+00            /     3.0D+00 &
          + 2.0D+00 * theta**2 /    15.0D+00 &
          - 4.0D+00 * theta**4 /   105.0D+00 &
          + 2.0D+00 * theta**6 /   567.0D+00 &
          - 4.0D+00 * theta**8 / 22275.0D+00

    gamma = 4.0D+00            /      3.0D+00 &
          - 2.0D+00 * theta**2 /     15.0D+00 &
          +           theta**4 /    210.0D+00 &
          -           theta**6 /  11340.0D+00

  else
 
    alpha = ( theta**2 + theta * sint * cost &
      - 2.0D+00 * sint**2 ) / theta**3

    beta = ( 2.0D+00 * theta + 2.0D+00 * theta * cost**2 &
      - 4.0D+00 * sint * cost ) / theta**3

    gamma = 4.0D+00 * ( sint - theta * cost ) / theta**3
 
  end if
  
  s2n = sum ( ftab(1:n:2) * sin ( t * x(1:n:2) ) ) &
    - 0.5D+00 * ( ftab(n) * sin ( t * x(n) ) &
                + ftab(1) * sin ( t * x(1) ) )

  s2nm1 = sum ( ftab(2:n-1:2) * sin ( t * x(2:n-1:2) ) )

  value = h * ( &
      alpha * ( ftab(1) * cos ( t * x(1) ) &
              - ftab(n) * cos ( t * x(n) ) ) &
    + beta * s2n &
    + gamma * s2nm1 )
 
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
!    18 May 2013
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

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
