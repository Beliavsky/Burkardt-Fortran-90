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
  logical ( kind = 4 ) lopen

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
subroutine lobatto_polynomial_derivative ( m, n, x, lp )

!*****************************************************************************80
!
!! LOBATTO_POLYNOMIAL_DERIVATIVE: derivative of completed Lobatto polynomial.
!
!  Discussion:
!
!    L(N,X)  =  N * ( P(N-1,X) - X * P(N,X) ) 
!    L'(N,X) =  N * ( P'(N-1,X) - P(N,X) - X * P'(N,X) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 November 2014
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
!    Larry Andrews,
!    Special Functions of Mathematics for Engineers,
!    Second Edition,
!    Oxford University Press, 1998,
!    ISBN: 0819426164,
!    LC: QA351.A75.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of evaluation points.
!
!    Input, integer ( kind = 4 ) N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Input, real ( kind = 8 ) X(M), the evaluation points.
!
!    Output, real ( kind = 8 ) LP(M,N), the derivative of the completed Lobatto
!    polynomials of order 1 through N at the point X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  real ( kind = 8 ) lp(m,n)
  real ( kind = 8 ) p(m,n+1)
  real ( kind = 8 ) pp(m,n+1)
  real ( kind = 8 ) x(m)

  if ( 1 <= n ) then

    lp(1:m,1) = - 2.0D+00 * x(1:m)

    if ( 2 <= n ) then

      p(1:m,1) = 1.0D+00
      p(1:m,2) = x(1:m)
      do j = 2, n
        p(1:m,j+1) = &
          ( real ( 2 * j - 1, kind = 8 ) * x(1:m) * p(1:m,j)     &
          - real (     j - 1, kind = 8 ) *          p(1:m,j-1) ) &
          / real (     j,     kind = 8 )
      end do

      pp(1:m,1) = 0.0D+00
      pp(1:m,2) = 1.0D+00
      do j = 2, n
        pp(1:m,j+1) = &
          ( real ( 2 * j - 1, kind = 8 ) * ( p(1:m,j) + x(1:m) * pp(1:m,j) )   &
          - real (     j - 1, kind = 8 ) *                       pp(1:m,j-1) ) &
          / real (     j,     kind = 8 )
      end do

      do j = 2, n
        lp(1:m,j) = &
          real ( j, kind = 8 ) &
          * ( pp(1:m,j) - p(1:m,j+1) - x(1:m) * pp(1:m,j+1) )
      end do

    end if

  end if

  return
end
subroutine lobatto_polynomial_derivatives ( n_data, n, x, fx )

!*****************************************************************************80
!
!! LOBATTO_POLYNOMIAL_DERIVATIVES: derivatives of completed Lobatto polynomials.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      n * LegendreP [ n - 1, x ] - n * x * LegendreP [ n, x ]
!
!    In Mathematica, the completed Lobatto polynomial can be evaluated by:
!
!       n * LegendreP [ n - 1, x ] - n * x * LegendreP [ n, x ]
!
!    The derivative is:
!
!         n * D[LegendreP [ n - 1, x ], {x} ] 
!       - n * LegendreP [ n, x ] 
!       - n * x * D[LegendreP [ n, x ], {x} ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) N, the order of the function.
!
!    Output, real ( kind = 8 ) X, the point where the function is evaluated.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 31

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
        -0.5D+00, &
         2.437500000000000D+00, &
         4.031250000000000D+00, &
        -3.154296875000000D+00, &
       -10.19165039062500D+00, &
        -1.019622802734375D+00, &
        15.67544555664063D+00, &
        10.97668933868408D+00, &
       -15.91419786214828D+00, &
       -24.33202382177114D+00, &
        12.00000000000000D+00, &
         5.670000000000000D+00, &
         0.9600000000000000D+00, &
        -2.310000000000000D+00, &
        -4.320000000000000D+00, &
        -5.250000000000000D+00, &
        -5.280000000000000D+00, &
        -4.590000000000000D+00, &
        -3.360000000000000D+00, &
        -1.770000000000000D+00, &
         0.0D+00, &
         1.770000000000000D+00, &
         3.360000000000000D+00, &
         4.590000000000000D+00, &
         5.280000000000000D+00, &
         5.250000000000000D+00, &
         4.320000000000000D+00, &
         2.310000000000000D+00, &
        -0.9600000000000000D+00, &
        -5.670000000000000D+00, &
       -12.00000000000000D+00 /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ), save, dimension ( n_max ) :: n_vec = (/ &
     1,  2, &
     3,  4,  5, &
     6,  7,  8, &
     9, 10,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3,  3 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
   -1.00D+00, &
   -0.90D+00, &
   -0.80D+00, &
   -0.70D+00, &
   -0.60D+00, &
   -0.50D+00, &
   -0.40D+00, &
   -0.30D+00, &
   -0.20D+00, &
   -0.10D+00, &
    0.00D+00, &
    0.10D+00, &
    0.20D+00, &
    0.30D+00, &
    0.40D+00, &
    0.50D+00, &
    0.60D+00, &
    0.70D+00, &
    0.80D+00, &
    0.90D+00, &
    1.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    n = n_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine lobatto_polynomial_plot ( ndx_num, ndx, prefix )

!*****************************************************************************80
!
!! LOBATTO_POLYNOMIAL_PLOT plots one or more completed Lobatto polynomials.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 November 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NDX_NUM, the number of polynomials to plot.
!
!    Input, integer ( kind = 4 ) NDX(NDX_NUM), the orders of 1 or more 
!    Legendre polynomials to be plotted together.
!
!    Input, character ( len = * ) PREFIX. the filename prefix.
!
  implicit none

  integer ( kind = 4 ) ndx_num
  integer ( kind = 4 ), parameter :: x_num = 501

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ), allocatable :: l(:,:)
  real ( kind = 8 ), allocatable :: lp(:,:)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ndx(ndx_num)
  character ( len = 255 ) plot_filename
  character ( len = * ) prefix
  real ( kind = 8 ) x(x_num)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo
  real ( kind = 8 ) y(x_num,ndx_num)
  real ( kind = 8 ) yp(x_num,ndx_num)

  x_lo = -1.0D+00
  x_hi = +1.0D+00
  call r8vec_linspace ( x_num, x_lo, x_hi, x )
!
!  Collect the data.
!
  do j = 1, ndx_num

    n = ndx(j)

    allocate ( l(1:x_num,1:n) )
    call lobatto_polynomial_value ( x_num, n, x, l )
    y(1:x_num,j) = l(1:x_num,n)
    deallocate ( l )

    allocate ( lp(1:x_num,1:n) )
    call lobatto_polynomial_derivative ( x_num, n, x, lp )
    yp(1:x_num,j) = lp(1:x_num,n)
    deallocate ( lp )

  end do

  write ( *, '(a)' ) ''
!
!  Make data file for values.
!
  call get_unit ( data_unit )

  data_filename = trim ( prefix ) // '_value_data.txt'

  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, x_num
    write ( data_unit, '(g14.6)', advance = 'no' ) x(i)
    do j = 1, ndx_num
      write ( data_unit, '(2x,g14.6)', advance = 'no' ) y(i,j)
    end do
    write ( data_unit, '(a)' ) ''
  end do
  close ( unit = data_unit )

  write ( *, '(a)' ) '  Lobatto value data in "' &
    // trim ( data_filename ) // '".'
!
!  Make command file for values.
!
  call get_unit ( command_unit )

  command_filename = trim ( prefix ) // '_value_commands.txt'

  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set timestamp'
  plot_filename = trim ( prefix ) // '_value.png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "x"'
  write ( command_unit, '(a)' ) 'set ylabel "L(n,x)"'
  write ( command_unit, '(a)' ) 'set title "Lobatto values"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  do j = 1, ndx_num
    if ( j == 1 ) then
      write ( command_unit, '(a,i2,a)', advance = 'no' ) 'plot "'
    else
      write ( command_unit, '(a,i2,a)', advance = 'no' ) '     "'
    end if
    write ( command_unit, '(a,i2,a)', advance = 'no' ) &
      trim ( data_filename ) // '" using 1:', j + 1
    if ( j < ndx_num ) then
      write ( command_unit, '(a)', advance = 'no' ) ', \'
    end if
    write ( command_unit, '(a)' ) ''
  end do
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Lobatto value commands in "' &
    // trim ( command_filename ) // '".'
!
!  Make data file for derivatives.
!
  call get_unit ( data_unit )

  data_filename = trim ( prefix ) // '_derivative_data.txt'

  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do i = 1, x_num
    write ( data_unit, '(g14.6)', advance = 'no' ) x(i)
    do j = 1, ndx_num
      write ( data_unit, '(2x,g14.6)', advance = 'no' ) yp(i,j)
    end do
    write ( data_unit, '(a)' ) ''
  end do
  close ( unit = data_unit )

  write ( *, '(a)' ) '  Lobatto derivative data stored in "' &
    // trim ( data_filename ) // '".'
!
!  Make command file for derivatives.
!
  call get_unit ( command_unit )

  command_filename = trim ( prefix ) // '_derivative_commands.txt'

  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set timestamp'
  plot_filename = trim ( prefix ) // '_derivative.png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "x"'
  write ( command_unit, '(a)' ) 'set ylabel "L(n,x)"'
  write ( command_unit, '(a)' ) 'set title "Lobatto derivatives"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  
  do j = 1, ndx_num
    if ( j == 1 ) then
      write ( command_unit, '(a,i2,a)', advance = 'no' ) 'plot "'
    else
      write ( command_unit, '(a,i2,a)', advance = 'no' ) '     "'
    end if
    write ( command_unit, '(a,i2,a)', advance = 'no' ) &
      trim ( data_filename ) // '" using 1:', j + 1
    if ( j < ndx_num ) then
      write ( command_unit, '(a)', advance = 'no' ) ', \'
    end if
    write ( command_unit, '(a)' ) ''
  end do

  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Lobatto derivative commands in "' &
    // trim ( command_filename ) // '".'

  return
end
subroutine lobatto_polynomial_value ( m, n, x, l )

!*****************************************************************************80
!
!! LOBATTO_POLYNOMIAL_VALUE evaluates the completed Lobatto polynomials Lo(n,x).
!
!  Discussion:
!
!    L(N,X) = ( 1 - X^2 ) * P'(N,X)
!           = N * ( P(N-1,X) - X * P(N,X) ) 
!
!    The Lobatto polynomials are 0 at -1 and +1.
!
!      (1-x^2) * 1
!      (1-x^2) * 3X
!      (1-x^2) * ( -3 + 15x^2 ) / 2
!      (1-x^2) * ( -60x + 140x^3 ) / 8
!      (1-x^2) * ( -15 - 210x^2 + 315x^4 ) / 8
!      (1-x^2) * ( 210x - 1260x^3 + 1386x^5 ) / 16
!      (1-x^2) * ( -35 + 945x^2 - 3465x^4 + 3003x^6 ) / 16
!      (1-x^2) * ( -2520x + 27720x^3 - 72072x^5 + 51480x^7 ) / 128
!      (1-x^2) * ( 315 - 13860x^2 + 90090x^4 - 180180x^6 + 109395x^8 ) / 128
!      (1-x^2) * ( 6930x - 120120x^3 + 540540x^5 - 875160x^7 + 461890x^9 ) / 256
!
!    Mathematica: (replacing "n" by desired index):
!
!      Expand [ ( 1-x^2) * D [ LegendreP[n,x], {x} ] ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 November 2014
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
!    Larry Andrews,
!    Special Functions of Mathematics for Engineers,
!    Second Edition,
!    Oxford University Press, 1998,
!    ISBN: 0819426164,
!    LC: QA351.A75.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of evaluation points.
!
!    Input, integer ( kind = 4 ) N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Input, real ( kind = 8 ) X(M), the evaluation points.
!
!    Output, real ( kind = 8 ) L(M,N), the values of the completed Lobatto
!    polynomials of order 1 through N at the point X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  real ( kind = 8 ) l(m,n)
  real ( kind = 8 ) p(m,n+1)
  real ( kind = 8 ) x(m)

  l(1:m,1:n) = 0.0D+00

  if ( 1 <= n ) then

    l(1:m,1) = 1.0D+00 - x(1:m) ** 2

    if ( 2 <= n ) then

      p(1:m,1) = 1.0D+00
      p(1:m,2) = x(1:m)

      do j = 2, n
        p(1:m,j+1) = &
          ( real ( 2 * j - 1, kind = 8 ) * x(1:m) * p(1:m,j)     &
          - real (     j - 1, kind = 8 ) *          p(1:m,j-1) ) &
          / real (     j,     kind = 8 )
      end do

      do j = 2, n
        l(1:m,j) = real ( j, kind = 8 ) * ( p(1:m,j) - x(1:m) * p(1:m,j+1) )
      end do

    end if

  end if

  return
end
subroutine lobatto_polynomial_values ( n_data, n, x, fx )

!*****************************************************************************80
!
!! LOBATTO_POLYNOMIAL_VALUES: values of the completed Lobatto polynomials.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      n * LegendreP [ n - 1, x ] - n * x * LegendreP [ n, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) N, the order of the function.
!
!    Output, real ( kind = 8 ) X, the point where the function is evaluated.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 31

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.9375000000000000D+00, &
    0.7031250000000000D+00, &
   -0.9667968750000000D+00, &
   -1.501464843750000D+00, &
    0.3639221191406250D+00, &
    2.001914978027344D+00, &
    0.6597948074340820D+00, &
   -1.934441328048706D+00, &
   -1.769941113889217D+00, &
    1.215243665501475D+00, &
    0.000000000000000D+00, &
    0.8692500000000000D+00, &
    1.188000000000000D+00, &
    1.109250000000000D+00, &
    0.7680000000000000D+00, &
    0.2812500000000000D+00, &
   -0.2520000000000000D+00, &
   -0.7507500000000000D+00, &
   -1.152000000000000D+00, &
   -1.410750000000000D+00, &
   -1.500000000000000D+00, &
   -1.410750000000000D+00, &
   -1.152000000000000D+00, &
   -0.7507500000000000D+00, &
   -0.2520000000000000D+00, &
    0.2812500000000000D+00, &
    0.7680000000000000D+00, &
    1.109250000000000D+00, &
    1.188000000000000D+00, &
    0.8692500000000000D+00, &
    0.000000000000000D+00 /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ), save, dimension ( n_max ) :: n_vec = (/ &
     1,  2, &
     3,  4,  5, &
     6,  7,  8, &
     9, 10,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3,  3,  3, &
     3,  3 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
    0.25D+00, &
   -1.00D+00, &
   -0.90D+00, &
   -0.80D+00, &
   -0.70D+00, &
   -0.60D+00, &
   -0.50D+00, &
   -0.40D+00, &
   -0.30D+00, &
   -0.20D+00, &
   -0.10D+00, &
    0.00D+00, &
    0.10D+00, &
    0.20D+00, &
    0.30D+00, &
    0.40D+00, &
    0.50D+00, &
    0.60D+00, &
    0.70D+00, &
    0.80D+00, &
    0.90D+00, &
    1.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    n = n_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

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
