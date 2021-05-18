subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
subroutine naca4_cambered ( m, p, t, c, n, xc, xu, yu, xl, yl )

!*****************************************************************************80
!
!! NACA4_CAMBERED: (xu,yu), (xl,yl) for a NACA cambered 4-digit airfoil.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eastman Jacobs, Kenneth Ward, Robert Pinkerton,
!    "The characteristics of 78 related airfoil sections from tests in
!    the variable-density wind tunnel",
!    NACA Report 460, 1933.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) M, the maximum camber.
!    0.0 < M.
!
!    Input, real ( kind = 8 ) P, the location of maximum camber.
!    0.0 < P < 1.0
!
!    Input, real ( kind = 8 ) T, the maximum relative thickness.
!    0.0 < T <= 1.0
!
!    Input, real ( kind = 8 ) C, the chord length.
!    0.0 < C.
!
!    Input, integer ( kind = 4 ) N, the number of sample points.
!
!    Input, real ( kind = 8 ) XC(N), points along the chord length.  
!    0.0 <= XC(*) <= C.
!
!    Output, real ( kind = 8 ) XU(N), YU(N), XL(N), YL(N), for each value of 
!    XC, measured along the camber line, the corresponding values (XU,YU) 
!    on the upper airfoil surface and (XL,YL) on the lower airfoil surface.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) c
  real ( kind = 8 ) divisor
  real ( kind = 8 ) dycdx
  integer ( kind = 4 ) i
  real ( kind = 8 ) m
  real ( kind = 8 ) p
  real ( kind = 8 ) t
  real ( kind = 8 ) theta
  real ( kind = 8 ) xc(n)
  real ( kind = 8 ) xl(n)
  real ( kind = 8 ) xu(n)
  real ( kind = 8 ) yc
  real ( kind = 8 ) yl(n)
  real ( kind = 8 ) yt
  real ( kind = 8 ) yu(n)

  do i = 1, n

    if ( 0.0D+00 <= xc(i) / c .and. xc(i) / c <= p ) then
      divisor = p ** 2
    else if ( p <= xc(i) / c .and. xc(i) / c <= 1.0D+00 ) then
      divisor = ( 1.0D+00 - p ) ** 2
    else
      divisor = 1.0D+00
    end if

    dycdx = 2.0D+00 * m * ( p - xc(i) / c ) / divisor

    theta = atan ( dycdx )
   
    yt = 5.0D+00 * t * c * ( &
       0.2969D+00 * sqrt ( xc(i) / c ) &
       + (((( &
         - 0.1015D+00 ) * ( xc(i) / c ) &
         + 0.2843D+00 ) * ( xc(i) / c ) &
         - 0.3516D+00 ) * ( xc(i) / c ) &
         - 0.1260D+00 ) * ( xc(i) / c ) )

    if ( 0.0D+00 <= xc(i) / c .and. xc(i) / c <= p ) then
      yc = m * xc(i) * ( 2.0D+00 * p - xc(i) / c ) / p ** 2
    else if ( p <= xc(i) / c .and. xc(i) / c <= 1.0D+00 ) then
      yc = m * ( xc(i) - c ) * ( 2.0D+00 * p - xc(i) / c - 1.0D+00 ) &
        / ( 1.0D+00 - p ) ** 2
    else
      yc = 0.0D+00
    end if

    xu(i) = xc(i) - yt * sin ( theta )
    yu(i) = yc + yt * cos ( theta )
    xl(i) = xc(i) + yt * sin ( theta )
    yl(i) = yc - yt * cos ( theta )

  end do

  return
end
subroutine naca4_symmetric ( t, c, n, x, y )

!*****************************************************************************80
!
!! NACA4_SYMMETRIC evaluates y(x) for a NACA symmetric 4-digit airfoil.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eastman Jacobs, Kenneth Ward, Robert Pinkerton,
!    "The characteristics of 78 related airfoil sections from tests in
!    the variable-density wind tunnel",
!    NACA Report 460, 1933.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T, the maximum relative thickness.
!
!    Input, real ( kind = 8 ) C, the chord length.
!
!    Input, integer ( kind = 4 ) N, the number of sample points.
!
!    Input, real ( kind = 8 ) X(N), points along the chord length.  
!    0.0 <= X(*) <= C.
!
!    Output, real ( kind = 8 ) Y(N), for each value of X, the corresponding
!    value of Y so that (X,Y) is on the upper wing surface, and (X,-Y) is on the
!    lower wing surface.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) c
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  y(1:n) = 5.0D+00 * t * c * ( &
    0.2969D+00 * sqrt ( x(1:n) / c ) &
    + (((( &
      - 0.1015D+00 ) * ( x(1:n) / c ) &
      + 0.2843D+00 ) * ( x(1:n) / c ) &
      - 0.3516D+00 ) * ( x(1:n) / c ) &
      - 0.1260D+00 ) * ( x(1:n) / c ) )

  return
end
subroutine r8mat_write ( output_filename, m, n, table )

!*****************************************************************************80
!
!! R8MAT_WRITE writes an R8MAT file.
!
!  Discussion:
!
!    An R8MAT is an array of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) OUTPUT_FILENAME, the output file name.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) TABLE(M,N), the data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  character ( len = * ) output_filename
  integer ( kind = 4 ) output_status
  integer ( kind = 4 ) output_unit
  character ( len = 30 ) string
  real ( kind = 8 ) table(m,n)
!
!  Open the file.
!
  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_filename, &
    status = 'replace', iostat = output_status )

  if ( output_status /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_WRITE - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the output file "' // &
      trim ( output_filename ) // '" on unit ', output_unit
    output_unit = -1
    stop 1
  end if
!
!  Create a format string.
!
!  For less precision in the output file, try:
!
!                                            '(', m, 'g', 14, '.', 6, ')'
!
  if ( 0 < m .and. 0 < n ) then

    write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'g', 24, '.', 16, ')'
!
!  Write the data.
!
    do j = 1, n
      write ( output_unit, string ) table(1:m,j)
    end do

  end if
!
!  Close the file.
!
  close ( unit = output_unit )

  return
end
subroutine r8vec_linspace ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_LINSPACE creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
!
!    In other words, the interval is divided into N-1 even subintervals,
!    and the endpoints of intervals are used as the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the first and last entries.
!
!    Output, real ( kind = 8 ) X(N), a vector of linearly spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n
      x(i) = ( real ( n - i,     kind = 8 ) * a   &
             + real (     i - 1, kind = 8 ) * b ) &
             / real ( n     - 1, kind = 8 )
    end do

  end if

  return
end
function r8vec_max ( n, a )

!*****************************************************************************80
!
!! R8VEC_MAX returns the maximum value in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) R8VEC_MAX, the value of the largest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_max
  real ( kind = 8 ) value

  value = maxval ( a(1:n) )

  r8vec_max = value

  return
end
function r8vec_min ( n, a )

!*****************************************************************************80
!
!! R8VEC_MIN returns the minimum value of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) R8VEC_MIN, the value of the smallest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_min
  real ( kind = 8 ) value

  value = minval ( a(1:n) )

  r8vec_min = value

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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
