subroutine chebyshev1 ( n, x )

!*****************************************************************************80
!
!! CHEBYSHEV1 returns the Type 1 Chebyshev points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2018
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) angle(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)

  do i = 1, n
    angle(i) = r8_pi * real ( 2 * ( i - 1 ) + 1, kind = 8 ) &
                     / real ( 2 * n, kind = 8 )
  end do

  x(1:n) = cos ( angle(1:n) )

  return
end
subroutine chebyshev2 ( n, x )

!*****************************************************************************80
!
!! CHEBYSHEV2 returns the Type 2 Chebyshev points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2018
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) angle(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then
    x(1) = 0.0D+00
  else
    do i = 1, n
      angle(i) = r8_pi * real ( n - i, kind = 8 ) / real ( n - 1, kind = 8 )
    end do
    x(1:n) = cos ( angle(1:n) )
  end if

  return
end
subroutine chebyshev3 ( n, x )

!*****************************************************************************80
!
!! CHEBYSHEV3 returns the Type 3 Chebyshev points.
!
!  Discussion:
!
!    Note that this point set is NOT symmetric in [-1,+1].
!    It is sometimes augmented by the value -1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2018
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) angle(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)

  do i = 1, n
    angle(i) = r8_pi * real ( 2 * n - 2 * i + 1, kind = 8 ) &
                     / real ( 2 * n         + 1, kind = 8 )
  end do

  x(1:n) = cos ( angle(1:n) )

  return
end
subroutine chebyshev4 ( n, x )

!*****************************************************************************80
!
!! CHEBYSHEV4 returns the Type 4 Chebyshev points.
!
!  Discussion:
!
!    Note that this point set is NOT symmetric in [-1,+1].
!    It is sometimes augmented by the value +1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2018
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) angle(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(n)

  do i = 1, n
    angle(i) = r8_pi * real ( 2 * ( n - i + 1 ), kind = 8 ) &
                     / real ( 2 * n + 1, kind = 8 )
  end do

  x(1:n) = cos ( angle(1:n) )

  return
end
subroutine equidistant1 ( n, x )

!*****************************************************************************80
!
!! EQUIDISTANT1 returns the Type 1 Equidistant points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2018
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the points.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, n
    x(i) = real ( - n  - 1 + 2 * i, kind = 8 ) / real ( n + 1, kind = 8 )
  end do

  return
end
subroutine equidistant2 ( n, x )

!*****************************************************************************80
!
!! EQUIDISTANT2 returns the Type 2 Equidistant points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2018
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the points.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then
    x(1) = 0.0D+00
  else
    do i = 1, n
      x(i) = real ( - n  - 1 + 2 * i, kind = 8 ) / real ( n - 1, kind = 8 )
    end do
  end if

  return
end
subroutine equidistant3 ( n, x )

!*****************************************************************************80
!
!! EQUIDISTANT3 returns the Type 3 Equidistant points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2018
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the points.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, n
    x(i) = real ( - n  - 1 + 2 * i, kind = 8 ) / real ( n, kind = 8 )
  end do

  return
end
subroutine fejer1 ( n, x )

!*****************************************************************************80
!
!! FEJER1 returns the Type 1 Fejer points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2018
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the points.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) theta(n)
  real ( kind = 8 ) x(n)

  do i = 1, n
    theta(i) = r8_pi * real ( 2 * n + 1 - 2 * i, kind = 8 ) &
                     / real ( 2 * n, kind = 8 )
  end do

  x(1:n) = cos ( theta(1:n) )

  return
end
subroutine fejer2 ( n, x )

!*****************************************************************************80
!
!! FEJER2 returns the Type 2 Fejer points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2018
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) X(N), the points.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) theta(n)
  real ( kind = 8 ) x(n)

  do i = 1, n
    theta(i) = r8_pi * real ( n + 1 - i, kind = 8 ) &
                     / real ( n + 1, kind = 8 )
  end do

  x(1:n) = cos ( theta(1:n) )

  return
end
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
subroutine lagrange_value ( data_num, t_data, interp_num, t_interp, l_interp )

!*****************************************************************************80
!
!! LAGRANGE_VALUE evaluates the Lagrange polynomials.
!
!  Discussion:
!
!    Given DATA_NUM distinct abscissas, T_DATA(1:DATA_NUM),
!    the I-th Lagrange polynomial L(I)(T) is defined as the polynomial of
!    degree DATA_NUM - 1 which is 1 at T_DATA(I) and 0 at the DATA_NUM - 1
!    other abscissas.
!
!    A formal representation is:
!
!      L(I)(T) = Product ( 1 <= J <= DATA_NUM, I /= J )
!       ( T - T(J) ) / ( T(I) - T(J) )
!
!    This routine accepts a set of INTERP_NUM values at which all the Lagrange
!    polynomials should be evaluated.
!
!    Given data values P_DATA at each of the abscissas, the value of the
!    Lagrange interpolating polynomial at each of the interpolation points
!    is then simple to compute by matrix multiplication:
!
!      P_INTERP(1:INTERP_NUM) =
!        P_DATA(1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
!
!    or, in the case where P is multidimensional:
!
!      P_INTERP(1:M,1:INTERP_NUM) =
!        P_DATA(1:M,1:DATA_NUM) * L_INTERP(1:DATA_NUM,1:INTERP_NUM)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DATA_NUM, the number of data points.
!    DATA_NUM must be at least 1.
!
!    Input, real ( kind = 8 ) T_DATA(DATA_NUM), the data points.
!
!    Input, integer ( kind = 4 ) INTERP_NUM, the number of
!    interpolation points.
!
!    Input, real ( kind = 8 ) T_INTERP(INTERP_NUM), the
!    interpolation points.
!
!    Output, real ( kind = 8 ) L_INTERP(DATA_NUM,INTERP_NUM), the values
!    of the Lagrange polynomials at the interpolation points.
!
  implicit none

  integer ( kind = 4 ) data_num
  integer ( kind = 4 ) interp_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) l_interp(data_num,interp_num)
  real ( kind = 8 ) t_data(data_num)
  real ( kind = 8 ) t_interp(interp_num)
!
!  Evaluate the polynomial.
!
  l_interp(1:data_num,1:interp_num) = 1.0D+00

  do i = 1, data_num

    do j = 1, data_num

      if ( j /= i ) then

        l_interp(i,1:interp_num) = l_interp(i,1:interp_num) &
          * ( t_interp(1:interp_num) - t_data(j) ) / ( t_data(i) - t_data(j) )

      end if

    end do

  end do

  return
end
subroutine lebesgue_constant ( n, x, nfun, xfun, lmax )

!*****************************************************************************80
!
!! LEBESGUE_CONSTANT estimates the Lebesgue constant for a set of points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2014
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Jean-Paul Berrut, Lloyd Trefethen,
!    Barycentric Lagrange Interpolation,
!    SIAM Review,
!    Volume 46, Number 3, September 2004, pages 501-517.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of interpolation points.
!
!    Input, real ( kind = 8 ) X(N), the interpolation points.
!
!    Input, integer ( kind = 4 ) NFUN, the number of evaluation points.
!
!    Input, real ( kind = 8 ) XFUN(*), the evaluation points.
!
!    Output, real ( kind = 8 ) LMAX, an estimate of the Lebesgue constant 
!    for the points.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) nfun

  real ( kind = 8 ) lfun(nfun)
  real ( kind = 8 ) lmax
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xfun(nfun)

  call lebesgue_function ( n, x, nfun, xfun, lfun )

  lmax = maxval ( lfun )

  return
end
subroutine lebesgue_function ( n, x, nfun, xfun, lfun )

!*****************************************************************************80
!
!! LEBESGUE_FUNCTION evaluates the Lebesgue function for a set of points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2014
!
!  Author:
!
!    Original MATLAB version by Greg von Winckel.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Jean-Paul Berrut, Lloyd Trefethen,
!    Barycentric Lagrange Interpolation,
!    SIAM Review,
!    Volume 46, Number 3, September 2004, pages 501-517.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of interpolation points.
!
!    Input, real ( kind = 8 ) X(N), the interpolation points.
!
!    Input, integer ( kind = 4 ) NFUN, the number of evaluation points.
!
!    Input, real ( kind = 8 ) XFUN(NFUN), the evaluation points.
!
!    Output, real ( kind = 8 ) LFUN(NFUN), the Lebesgue function values.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) nfun

  integer ( kind = 4 ) j
  real ( kind = 8 ) lfun(nfun)
  real ( kind = 8 ) llfun(n,nfun)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xfun(nfun)
!
!  Handle special case.
!
  if ( n == 1 ) then
    lfun(1:nfun) = 1.0D+00
    return
  end if

  call lagrange_value ( n, x, nfun, xfun, llfun ) 

  do j = 1, nfun
    lfun(j) = sum ( abs ( llfun(1:n,j) ) )
  end do

  return
end
subroutine lebesgue_plot ( n, x, nfun, xfun, label, filename )

!*****************************************************************************80
!
!! LEBESGUE_PLOT plots the Lebesgue function for a set of points.
!
!  Discussion:
!
!    The interpolation interval is assumed to be [min(XFUN), max(XFUN)].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2014
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Jean-Paul Berrut, Lloyd Trefethen,
!    Barycentric Lagrange Interpolation,
!    SIAM Review,
!    Volume 46, Number 3, September 2004, pages 501-517.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of interpolation points.
!
!    Input, real ( kind = 8 ) X(N), the interpolation points.
!
!    Input, integer ( kind = 4 ) NFUN, the number of evaluation points.
!
!    Input, real ( kind = 8 ) XFUN(NFUN), the evaluation points.  
!
!    Input, character ( len = * ) LABEL, a title for the plot.
!
!    Input, character ( len = * ) FILENAME, a partial filename.
!    The program will create "filename_commands.txt', 'filename_data.txt',
!    and 'filename.png'.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) nfun

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = * ) filename
  integer ( kind = 4 ) i
  character ( len = * ) label
  real ( kind = 8 ) lfun(nfun)
  character ( len = 255 ) png_filename
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xfun(nfun)

  call lebesgue_function ( n, x, nfun, xfun, lfun )
!
!  Create data file.
!
  data_filename = trim ( filename ) // '_data.txt'
  call get_unit ( data_unit )
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, nfun
    write ( data_unit, '(2x,g14.6,2x,g14.6)' ) xfun(i), lfun(i)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Created graphics data file "' // trim ( data_filename ) // '".'
!
!  Create command file.
!
  command_filename = trim ( filename ) // '_commands.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'

  png_filename = trim ( filename ) // '.png'
  write ( command_unit, '(a)' ) 'set output "'// trim ( png_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Lebesgue(X) --->"'
  write ( command_unit, '(a)' ) &
    'set title "' // trim ( label ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "red"'

  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created graphics command file "' // trim ( command_filename ) // '".'

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
!    Input, real ( kind = 8 ) A_FIRST, A_LAST, the first and last entries.
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
subroutine r8vec_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
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
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
  end do

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
