subroutine gamma_values ( n_data, x, fx )

!*****************************************************************************80
!
!! gamma_values returns some values of the Gamma function.
!
!  Discussion:
!
!    The Gamma function is defined as:
!
!      Gamma(Z) = integral ( 0 <= T < +oo) T^(Z-1) exp(-T) dT
!
!    It satisfies the recursion:
!
!      Gamma(X+1) = X * Gamma(X)
!
!    Gamma is undefined for nonpositive integral X.
!    Gamma(0.5) = sqrt(PI)
!    For N a positive integer, Gamma(N+1) = N!, the standard factorial.
!
!    In Mathematica, the function can be evaluated by:
!
!      Gamma[x]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 May 2007
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 before 
!    the first call.  
!
!  Output:
!
!    integer ( kind = 4 ) N_DATA.  On each call, the routine increments 
!    N_DATA by 1, and returns the corresponding data; when there is no 
!    more data, the output value of N_DATA will be 0 again.
!
!    real ( kind = 8 ) X, the argument of the function.
!
!    real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 25

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    -0.3544907701811032D+01, &
    -0.1005871979644108D+03, &
     0.9943258511915060D+02, &
     0.9513507698668732D+01, &
     0.4590843711998803D+01, &
     0.2218159543757688D+01, &
     0.1772453850905516D+01, &
     0.1489192248812817D+01, &
     0.1164229713725303D+01, &
     0.1000000000000000D+01, &
     0.9513507698668732D+00, &
     0.9181687423997606D+00, &
     0.8974706963062772D+00, &
     0.8872638175030753D+00, &
     0.8862269254527580D+00, &
     0.8935153492876903D+00, &
     0.9086387328532904D+00, &
     0.9313837709802427D+00, &
     0.9617658319073874D+00, &
     0.1000000000000000D+01, &
     0.2000000000000000D+01, &
     0.6000000000000000D+01, &
     0.3628800000000000D+06, &
     0.1216451004088320D+18, &
     0.8841761993739702D+31 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    -0.50D+00, &
    -0.01D+00, &
     0.01D+00, &
     0.10D+00, &
     0.20D+00, &
     0.40D+00, &
     0.50D+00, &
     0.60D+00, &
     0.80D+00, &
     1.00D+00, &
     1.10D+00, &
     1.20D+00, &
     1.30D+00, &
     1.40D+00, &
     1.50D+00, &
     1.60D+00, &
     1.70D+00, &
     1.80D+00, &
     1.90D+00, &
     2.00D+00, &
     3.00D+00, &
     4.00D+00, &
    10.00D+00, &
    20.00D+00, &
    30.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
subroutine gamma_log_values ( n_data, x, fx )

!*****************************************************************************80
!
!! GAMMA_LOG_VALUES returns some values of the Log Gamma function.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Log[Gamma[x]]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2004
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 before 
!    the first call.  
!
!  Output:
!
!    integer ( kind = 4 ) N_DATA.  On each call, the routine increments 
!    N_DATA by 1, and returns the corresponding data; when there is no 
!    more data, the output value of N_DATA will be 0 again.
!
!    real ( kind = 8 ) X, the argument of the function.
!
!    real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 20

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
     0.1524063822430784D+01, &
     0.7966778177017837D+00, &
     0.3982338580692348D+00, &
     0.1520596783998375D+00, &
     0.0000000000000000D+00, &
    -0.4987244125983972D-01, &
    -0.8537409000331584D-01, &
    -0.1081748095078604D+00, &
    -0.1196129141723712D+00, &
    -0.1207822376352452D+00, &
    -0.1125917656967557D+00, &
    -0.9580769740706586D-01, &
    -0.7108387291437216D-01, &
    -0.3898427592308333D-01, &
    0.00000000000000000D+00, &
    0.69314718055994530D+00, &
    0.17917594692280550D+01, &
    0.12801827480081469D+02, &
    0.39339884187199494D+02, &
    0.71257038967168009D+02 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
     0.20D+00, &
     0.40D+00, &
     0.60D+00, &
     0.80D+00, &
     1.00D+00, &
     1.10D+00, &
     1.20D+00, &
     1.30D+00, &
     1.40D+00, &
     1.50D+00, &
     1.60D+00, &
     1.70D+00, &
     1.80D+00, &
     1.90D+00, &
     2.00D+00, &
     3.00D+00, &
     4.00D+00, &
    10.00D+00, &
    20.00D+00, &
    30.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

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
!  Output:
!
!    integer ( kind = 4 ) IUNIT, the free unit number.
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
subroutine i4_fake_use ( n )

!*****************************************************************************80
!
!! i4_fake_use pretends to use a variable.
!
!  Discussion:
!
!    Some compilers will issue a warning if a variable is unused.
!    Sometimes there's a good reason to include a variable in a program,
!    but not to use it.  Calling this function with that variable as
!    the argument will shut the compiler up.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the variable to be "used".
!
  implicit none

  integer ( kind = 4 ) n

  if ( n /= n ) then
    write ( *, '(a)' ) '  i4_fake_use: variable is NAN.'
  end if

  return
end
function i4_log_10 ( i )

!*****************************************************************************80
!
!! I4_LOG_10 returns the integer part of the logarithm base 10 of an I4.
!
!  Discussion:
!
!    I4_LOG_10 ( I ) + 1 is the number of decimal digits in I.
!
!    An I4 is an integer ( kind = 4 ) value.
!
!  Example:
!
!        I  I4_LOG_10
!    -----  --------
!        0    0
!        1    0
!        2    0
!        9    0
!       10    1
!       11    1
!       99    1
!      100    2
!      101    2
!      999    2
!     1000    3
!     1001    3
!     9999    3
!    10000    4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 June 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) I, the number whose logarithm base 10
!    is desired.
!
!  Output:
!
!    integer ( kind = 4 ) I4_LOG_10, the integer part of the
!    logarithm base 10 of the absolute value of X.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_abs
  integer ( kind = 4 ) i4_log_10
  integer ( kind = 4 ) ten_pow

  if ( i == 0 ) then

    i4_log_10 = 0

  else

    i4_log_10 = 0
    ten_pow = 10

    i_abs = abs ( i )

    do while ( ten_pow <= i_abs )
      i4_log_10 = i4_log_10 + 1
      ten_pow = ten_pow * 10
    end do

  end if

  return
end
function i4_modp ( i, j )

!*****************************************************************************80
!
!! I4_MODP returns the nonnegative remainder of I4 division.
!
!  Discussion:
!
!    If
!      NREM = I4_MODP ( I, J )
!      NMULT = ( I - NREM ) / J
!    then
!      I = J * NMULT + NREM
!    where NREM is always nonnegative.
!
!    The MOD function computes a result with the same sign as the
!    quantity being divided.  Thus, suppose you had an angle A,
!    and you wanted to ensure that it was between 0 and 360.
!    Then mod(A,360) would do, if A was positive, but if A
!    was negative, your result would be between -360 and 0.
!
!    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
!
!    An I4 is an integer ( kind = 4 ) value.
!
!  Example:
!
!        I     J     MOD I4_MODP    Factorization
!
!      107    50       7       7    107 =  2 *  50 + 7
!      107   -50       7       7    107 = -2 * -50 + 7
!     -107    50      -7      43   -107 = -3 *  50 + 43
!     -107   -50      -7      43   -107 =  3 * -50 + 43
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) I, the number to be divided.
!
!    integer ( kind = 4 ) J, the number that divides I.
!
!  Output:
!
!    integer ( kind = 4 ) I4_MODP, the nonnegative remainder when I is
!    divided by J.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) j
  integer ( kind = 4 ) value

  if ( j == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4_MODP - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal divisor J = ', j
    stop 1
  end if

  value = mod ( i, j )

  if ( value < 0 ) then
    value = value + abs ( j )
  end if

  i4_modp = value

  return
end
function i4_uniform_ab ( a, b, seed )

!*****************************************************************************80
!
!! I4_UNIFORM_AB returns a scaled pseudorandom I4.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ) value.
!
!    The pseudorandom number will be scaled to be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Input:
!
!    integer ( kind = 4 ) A, B, the limits of the interval.
!
!    integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.
!
!  Output:
!
!    integer ( kind = 4 ) I4_UNIFORM_AB, a number between A and B.
!
!    integer ( kind = 4 ) SEED, the updated "seed" value.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) k
  real ( kind = 4 ) r
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) value

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r = real ( seed, kind = 4 ) * 4.656612875E-10
!
!  Scale R to lie between A-0.5 and B+0.5.
!
  r = ( 1.0E+00 - r ) * ( real ( min ( a, b ), kind = 4 ) - 0.5E+00 ) &
    +             r   * ( real ( max ( a, b ), kind = 4 ) + 0.5E+00 )
!
!  Use rounding to convert R to an integer between A and B.
!
  value = nint ( r, kind = 4 )

  value = max ( value, min ( a, b ) )
  value = min ( value, max ( a, b ) )

  i4_uniform_ab = value

  return
end
function i4_wrap ( ival, ilo, ihi )

!*****************************************************************************80
!
!! I4_WRAP forces an I4 to lie between given limits by wrapping.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ) value.
!
!    There appears to be a bug in the GFORTRAN compiler which can lead to
!    erroneous results when the first argument of I4_WRAP is an expression.
!    In particular:
!
!    do i = 1, 3
!      if ( test ) then
!        i4 = i4_wrap ( i + 1, 1, 3 )
!      end if
!    end do
!
!    was, when I = 3, returning I4 = 3.  So I had to replace this with
!
!    do i = 1, 3
!      if ( test ) then
!        i4 = i + 1
!        i4 = i4_wrap ( i4, 1, 3 )
!      end if
!    end do
!
!  Example:
!
!    ILO = 4, IHI = 8
!
!    I  Value
!
!    -2     8
!    -1     4
!     0     5
!     1     6
!     2     7
!     3     8
!     4     4
!     5     5
!     6     6
!     7     7
!     8     8
!     9     4
!    10     5
!    11     6
!    12     7
!    13     8
!    14     4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) IVAL, a value.
!
!    integer ( kind = 4 ) ILO, IHI, the desired bounds.
!
!  Output:
!
!    integer ( kind = 4 ) I4_WRAP, a "wrapped" version of the value.
!
  implicit none

  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) i4_wrap
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) value
  integer ( kind = 4 ) wide

  jlo = min ( ilo, ihi )
  jhi = max ( ilo, ihi )

  wide = jhi - jlo + 1

  if ( wide == 1 ) then
    value = jlo
  else
    value = jlo + i4_modp ( ival - jlo, wide )
  end if

  i4_wrap = value

  return
end
subroutine i4int_to_r8int ( imin, imax, i, rmin, rmax, r )

!*****************************************************************************80
!
!! I4INT_TO_R8INT maps an I4INT to an R8INT.
!
!  Discussion:
!
!    The formula used is:
!
!      R := RMIN + ( RMAX - RMIN ) * ( I - IMIN ) / ( IMAX - IMIN )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) IMIN, IMAX, the range.
!
!    integer ( kind = 4 ) I, the integer to be converted.
!
!    real ( kind = 8 ) RMIN, RMAX, the range.
!
!  Output:
!
!    real ( kind = 8 ) R, the corresponding value in [RMIN,RMAX].
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) imax
  integer ( kind = 4 ) imin
  real ( kind = 8 ) r
  real ( kind = 8 ) rmax
  real ( kind = 8 ) rmin

  if ( imax == imin ) then

    r = 0.5D+00 * ( rmin + rmax )

  else

    r = ( real ( imax - i,        kind = 8 ) * rmin   &
        + real (        i - imin, kind = 8 ) * rmax ) &
        / real ( imax     - imin, kind = 8 )

  end if

  return
end
subroutine i4mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! I4MAT_PRINT prints an I4MAT.
!
!  Discussion:
!
!    An I4MAT is a rectangular array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    integer ( kind = 4 ) A(M,N), the matrix to be printed.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  ilo = 1
  ihi = m
  jlo = 1
  jhi = n

  call i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

  return
end
subroutine i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! I4MAT_PRINT_SOME prints some of an I4MAT.
!
!  Discussion:
!
!    An I4MAT is a rectangular array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    integer ( kind = 4 ) A(M,N), an M by N matrix to be printed.
!
!    integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 10
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  character ( len = 8 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8)' ) j
    end do

    write ( *, '(''  Col '',10a8)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        write ( ctemp(j2), '(i8)' ) a(i,j)

      end do

      write ( *, '(i5,a,10a8)' ) i, ':', ( ctemp(j), j = 1, inc )

    end do

  end do

  return
end
subroutine i4vec_indicator0 ( n, a )

!*****************************************************************************80
!
!! I4VEC_INDICATOR0 sets an I4VEC to the indicator vector (0,1,2,...)
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!  Output:
!
!    integer ( kind = 4 ) A(N), the array.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i

  do i = 1, n
    a(i) = i - 1
  end do

  return
end
subroutine i4vec_indicator1 ( n, a )

!*****************************************************************************80
!
!! I4VEC_INDICATOR1 sets an I4VEC to the indicator vector (1,2,3,...)
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!  Output:
!
!    integer ( kind = 4 ) A(N), the array.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i

  do i = 1, n
    a(i) = i
  end do

  return
end
subroutine i4vec_permute ( n, p, a )

!*****************************************************************************80
!
!! I4VEC_PERMUTE permutes an I4VEC in place.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!    This routine permutes an array of integer "objects", but the same
!    logic can be used to permute an array of objects of any arithmetic
!    type, or an array of objects of any complexity.  The only temporary
!    storage required is enough to store a single object.  The number
!    of data movements made is N + the number of cycles of order 2 or more,
!    which is never more than N + N/2.
!
!  Example:
!
!    Input:
!
!      N = 5
!      P = (   2,   4,   5,   1,   3 )
!      A = (   1,   2,   3,   4,   5 )
!
!    Output:
!
!      A    = (   2,   4,   5,   1,   3 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of objects.
!
!    integer ( kind = 4 ) P(N), the permutation.  P(I) = J means
!    that the I-th element of the output array should be the J-th
!    element of the input array.
!
!    integer ( kind = 4 ) A(N), the array to be permuted.
!
!  Output:
!
!    integer ( kind = 4 ) A(N), the permuted array.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) a_temp
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) istart
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) perm1_check

  ierror = perm1_check ( n, p )

  if ( ierror .ne. 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'I4VEC_PERMUTE - Fatal error!'
    write ( *, '(a)' ) '  PERM1_CHECK returned error.'
    stop 1
  end if
!
!  Search for the next element of the permutation that has not been used.
!
  do istart = 1, n

    if ( p(istart) < 0 ) then

      cycle

    else if ( p(istart) == istart ) then

      p(istart) = - p(istart)
      cycle

    else

      a_temp = a(istart)
      iget = istart
!
!  Copy the new value into the vacated entry.
!
      do

        iput = iget
        iget = p(iget)

        p(iput) = - p(iput)

        if ( iget < 1 .or. n < iget ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'I4VEC_PERMUTE - Fatal error!'
          write ( *, '(a)' ) '  A permutation index is out of range.'
          write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
          stop 1
        end if

        if ( iget == istart ) then
          a(iput) = a_temp
          exit
        end if

        a(iput) = a(iget)

      end do

    end if

  end do
!
!  Restore the signs of the entries.
!
  p(1:n) = - p(1:n)

  return
end
subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of components of the vector.
!
!    integer ( kind = 4 ) A(N), the vector to be printed.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,a,2x,i12)' ) i, ':', a(i)
  end do

  return
end
subroutine i4vec_transpose_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_TRANSPOSE_PRINT prints an I4VEC "transposed".
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Example:
!
!    A = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 /)
!    TITLE = 'My vector:  '
!
!    My vector:
!        1    2    3    4    5
!        6    7    8    9   10
!       11
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of components of the vector.
!
!    integer ( kind = 4 ) A(N), the vector to be printed.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  do ilo = 1, n, 5
    ihi = min ( ilo + 5 - 1, n )
    write ( *, '(5i12)' ) a(ilo:ihi)
  end do

  return
end
subroutine legendre_zeros ( n, x )

!*****************************************************************************80
!
!! LEGENDRE_ZEROS computes the zeros of the Legendre polynomial of degree N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 June 2011
!
!  Author:
!
!    Original FORTRAN77 version by Philip Davis, Philip Rabinowitz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Philip Davis, Philip Rabinowitz,
!    Methods of Numerical Integration,
!    Second Edition,
!    Dover, 2007,
!    ISBN: 0486453391,
!    LC: QA299.3.D28.
!
!  Input:
!
!    integer ( kind = 4 ) N, the order.
!    0 < N.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the locations of the zeros.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) d1
  real ( kind = 8 ) d2pn
  real ( kind = 8 ) d3pn
  real ( kind = 8 ) d4pn
  real ( kind = 8 ) dp
  real ( kind = 8 ) dpn
  real ( kind = 8 ) e1
  real ( kind = 8 ) fx
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iback
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mp1mi
  integer ( kind = 4 ) ncopy
  integer ( kind = 4 ) nmove
  real ( kind = 8 ) p
  real ( kind = 8 ) pk
  real ( kind = 8 ) pkm1
  real ( kind = 8 ) pkp1
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) t
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x0
  real ( kind = 8 ) xtemp

  e1 = real ( n * ( n + 1 ), kind = 8 )

  m = ( n + 1 ) / 2

  do i = 1, m

    mp1mi = m + 1 - i

    t = real ( 4 * i - 1, kind = 8 ) * r8_pi &
      / real ( 4 * n + 2, kind = 8 )

    x0 = cos ( t ) * ( 1.0D+00 - ( 1.0D+00 - 1.0D+00 &
      / real ( n, kind = 8 ) ) &
      / real ( 8 * n * n, kind = 8 ) )

    pkm1 = 1.0D+00
    pk = x0

    do k = 2, n
      pkp1 = 2.0D+00 * x0 * pk - pkm1 - ( x0 * pk - pkm1 ) &
        / real ( k, kind = 8 )
      pkm1 = pk
      pk = pkp1
    end do

    d1 = real ( n, kind = 8 ) * ( pkm1 - x0 * pk )

    dpn = d1 / ( 1.0D+00 - x0 ) / ( 1.0D+00 + x0 )

    d2pn = ( 2.0D+00 * x0 * dpn - e1 * pk ) / ( 1.0D+00 - x0 ) &
      / ( 1.0D+00 + x0 )

    d3pn = ( 4.0D+00 * x0 * d2pn + ( 2.0D+00 - e1 ) * dpn ) &
      / ( 1.0D+00 - x0 ) / ( 1.0D+00 + x0 )

    d4pn = ( 6.0D+00 * x0 * d3pn + ( 6.0D+00 - e1 ) * d2pn ) &
      / ( 1.0D+00 - x0 ) / ( 1.0D+00 + x0 )

    u = pk / dpn
    v = d2pn / dpn
!
!  Initial approximation H:
!
    h = - u * ( 1.0D+00 + 0.5D+00 * u * ( v + u * ( v * v - d3pn / &
      ( 3.0D+00 * dpn ) ) ) )
!
!  Refine H using one step of Newton's method:
!
    p = pk + h * ( dpn + 0.5D+00 * h * ( d2pn + h / 3.0D+00 &
      * ( d3pn + 0.25D+00 * h * d4pn ) ) )

    dp = dpn + h * ( d2pn + 0.5D+00 * h * ( d3pn + h * d4pn / 3.0D+00 ) )

    h = h - p / dp

    xtemp = x0 + h

    x(mp1mi) = xtemp

    fx = d1 - h * e1 * ( pk + 0.5D+00 * h * ( dpn + h / 3.0D+00 &
      * ( d2pn + 0.25D+00 * h * ( d3pn + 0.2D+00 * h * d4pn ) ) ) )

  end do

  if ( mod ( n, 2 ) == 1 ) then
    x(1) = 0.0D+00
  end if
!
!  Shift the data up.
!
  nmove = ( n + 1 ) / 2
  ncopy = n - nmove

  do i = 1, nmove
    iback = n + 1 - i
    x(iback) = x(iback-ncopy)
  end do
!
!  Reflect values for the negative abscissas.
!
  do i = 1, n - nmove
    x(i) = - x(n+1-i)
  end do

  return
end
function perm0_check ( n, p )

!*****************************************************************************80
!
!! PERM0_CHECK checks a 0-based permutation.
!
!  Discussion:
!
!    The routine verifies that each of the integers from 0 to
!    to N-1 occurs among the N entries of the permutation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries.
!
!    integer ( kind = 4 ) P(N), the array to check.
!
!  Output:
!
!    integer ( kind = 4 ) PERM0_CHECK:
!    0, no error.
!    1, P is not a 0-based permutation.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) location
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) perm0_check
  integer ( kind = 4 ) value

  ierror = 0

  do value = 0, n - 1

    ierror = 1

    do location = 1, n
      if ( p(location) == value ) then
        ierror = 0
        exit
      end if
    end do

    if ( ierror /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PERM0_CHECK - Fatal error!'
      write ( *, '(a,i4)' ) '  Permutation is missing value ', value
      exit
    end if

  end do

  perm0_check = ierror

  return
end
subroutine perm0_uniform ( n, seed, p )

!*****************************************************************************80
!
!! PERM0_UNIFORM selects a random permutation of objects 0, ..., N-1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of objects to be permuted.
!
!    integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!  Output:
!
!    integer ( kind = 4 ) P(N), the permutation.  P(I) is the "new"
!    location of the object originally at I.
!
!    integer ( kind = 4 ) SEED, a updated seed.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) seed

  do i = 1, n
    p(i) = i - 1
  end do

  do i = 1, n - 1
    j = i4_uniform_ab ( i, n, seed )
    k    = p(i)
    p(i) = p(j)
    p(j) = k
  end do

  return
end
function perm1_check ( n, p )

!*****************************************************************************80
!
!! PERM1_CHECK checks a 1-based permutation.
!
!  Discussion:
!
!    The routine verifies that each of the integers from 1 to
!    to N occurs among the N entries of the permutation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 May 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries.
!
!    integer ( kind = 4 ) P(N), the array to check.
!
!  Output:
!
!    integer ( kind = 4 ) PERM1_CHECK:
!    0, P is a 1-based permutation.
!    1, P is not a 1-based permutation.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) location
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) perm1_check
  integer ( kind = 4 ) value

  ierror = 0

  do value = 1, n

    ierror = 1

    do location = 1, n
      if ( p(location) == value ) then
        ierror = 0
        exit
      end if
    end do

    if ( ierror /= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PERM1_CHECK - Fatal error!'
      write ( *, '(a,i4)' ) '  Permutation is missing value ', value
      exit
    end if

  end do

  perm1_check = ierror

  return
end
subroutine perm1_uniform ( n, seed, p )

!*****************************************************************************80
!
!! PERM1_UNIFORM selects a random permutation of objects 1, ..., N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 2008
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of objects to be permuted.
!
!    integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!  Output:
!
!    integer ( kind = 4 ) P(N), the permutation.  P(I) is the "new"
!    location of the object originally at I.
!
!    integer ( kind = 4 ) SEED, an updated seed.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) seed

  do i = 1, n
    p(i) = i
  end do

  do i = 1, n - 1
    j = i4_uniform_ab ( i, n, seed )
    k    = p(i)
    p(i) = p(j)
    p(j) = k
  end do

  return
end
function r8_abs ( x )

!*****************************************************************************80
!
!! R8_ABS returns the absolute value of an R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    FORTRAN90 supplies the ABS function, which should be used instead
!    of this function!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose absolute value is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8_ABS, the absolute value of X.
!
  implicit none

  real ( kind = 8 ) r8_abs
  real ( kind = 8 ) x

  if ( 0.0D+00 <= x ) then
    r8_abs = + x
  else
    r8_abs = - x
  end if

  return
end
function r8_acos ( c )

!*****************************************************************************80
!
!! R8_ACOS computes the arc cosine function, with argument truncation.
!
!  Discussion:
!
!    If you call your system ACOS routine with an input argument that is
!    even slightly outside the range [-1.0, 1.0 ], you may get an unpleasant
!    surprise (I did).
!
!    This routine simply truncates arguments outside the range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) C, the argument.
!
!  Output:
!
!    real ( kind = 8 ) R8_ACOS, an angle whose cosine is C.
!
  implicit none

  real ( kind = 8 ) c
  real ( kind = 8 ) c2
  real ( kind = 8 ) r8_acos

  c2 = c
  c2 = max ( c2, -1.0D+00 )
  c2 = min ( c2, +1.0D+00 )

  r8_acos = acos ( c2 )

  return
end
function r8_acosh ( x )

!*****************************************************************************80
!
!! R8_ACOSH returns the inverse hyperbolic cosine of a number.
!
!  Discussion:
!
!    One formula is:
!
!      R8_ACOSH = log ( X + SQRT ( X^2 - 1.0 ) )
!
!    but this formula suffers from roundoff and overflow problems.
!    The formula used here was recommended by W Kahan, as discussed
!    by Moler.
!
!    Applying the inverse function
!
!      Y = R8_ACOSH ( X )
!
!    implies that
!
!      X = COSH(Y) = 0.5 * ( EXP(Y) + EXP(-Y) ).
!
!    For every X greater than or equal to 1, there are two possible
!    choices Y such that X = COSH(Y), differing only in sign.  It
!    is usual to resolve this choice by taking the value of 
!    R8_ACOSH ( X ) to be nonnegative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Cleve Moler,
!    Trigonometry is a Complex Subject,
!    MATLAB News and Notes,
!    Summer 1998.
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose inverse hyperbolic 
!    cosine is desired.  X should be greater than or equal to 1.
!
!  Output:
!
!    real ( kind = 8 ) R8_ACOSH, the inverse hyperbolic cosine of 
!    X.  The principal value (that is, the positive value of the two ) 
!    is returned.
!
  implicit none

  real ( kind = 8 ) r8_acosh
  real ( kind = 8 ) x

  if ( x < 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_ACOSH - Fatal error!'
    write ( *, '(a)' ) '  Argument X must satisfy 1 <= X.'
    write ( *, '(a,g14.6)' ) '  The input X = ', x
    stop 1
  end if

  r8_acosh = 2.0D+00 * log ( &
      sqrt ( 0.5D+00 * ( x + 1.0D+00 ) ) &
    + sqrt ( 0.5D+00 * ( x - 1.0D+00 ) ) )

  return
end
function r8_add ( x, y )

!*****************************************************************************80
!
!! R8_ADD returns the sum of two R8's.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    FORTRAN90 supplies the + operator, which should generally be used instead
!    of this function!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, Y, the numbers to be added.
!
!  Output:
!
!    real ( kind = 8 ) R8_ADD, the sum.
!
  implicit none

  real ( kind = 8 ) r8_add
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  r8_add = x + y

  return
end
function r8_agm ( a, b )

!*****************************************************************************80
!
!! R8_AGM computes the arithmetic-geometric mean of A and B.
!
!  Discussion:
!
!    The AGM is defined for nonnegative A and B.
!
!    The AGM of numbers A and B is defined by by an iteration:
!
!      A(0) = A
!      B(0) = B
!
!      A(N+1) = ( A(N) + B(N) ) / 2
!      B(N+1) = sqrt ( A(N) * B(N) )
!
!    The two sequences both converge to AGM(A,B).  Convergence can be
!    assumed when the two values are sufficiently close.
!
!    In Mathematica, the AGM can be evaluated by
!
!      ArithmeticGeometricMean [ a, b ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 February 2008
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    real ( kind = 8 ) A, B, the arguments whose AGM is to be computed.
!    0 <= A, 0 <= B.
!
!  Output:
!
!    real ( kind = 8 ) R8_AGM, the arithmetic-geometric mean of A and B.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a1
  real ( kind = 8 ) a2
  real ( kind = 8 ) b
  real ( kind = 8 ) b1
  real ( kind = 8 ) b2
  integer ( kind = 4 ) it
  integer ( kind = 4 ), parameter :: it_max = 1000
  real ( kind = 8 ) r8_agm
  real ( kind = 8 ) tol

  if ( a < 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_AGM - Fatal error!'
    write ( *, '(a)' ) '  A < 0.'
    stop 1
  end if

  if ( b < 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_AGM - Fatal error!'
    write ( *, '(a)' ) '  B < 0.'
    stop 1
  end if

  if ( a == 0.0D+00 .or. b == 0.0D+00 ) then
    r8_agm = 0.0D+00
    return
  end if

  if ( a == b ) then
    r8_agm = a
    return
  end if

  it = 0
  tol = 100.0D+00 * epsilon ( tol )

  a1 = a
  b1 = b

  do

    it = it + 1

    a2 = ( a1 + b1 ) / 2.0D+00
    b2 = sqrt ( a1 * b1 )

    if ( abs ( a2 - b2 ) <= tol * ( a2 + b2 ) ) then
      exit
    end if

    if ( it_max < it ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_AGM - Fatal error!'
      write ( *, '(a,i8)' ) '  Exceeded iteration limit ', it_max
      write ( *, '(a,g14.6)' ) '  Estimated value = ', a2
      stop 1
    end if

    a1 = a2
    b1 = b2

  end do

  r8_agm = a2

  return
end
function r8_aint ( x )

!****************************************************************************80
!
!! R8_AINT truncates an R8 argument to an integer.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 October 2011
!
!  Author:
!
!    John Burkardt.
!
!  Input:
!
!    real ( kind = 8 ) X, the argument.
!
!  Output:
!
!    real ( kind = 8 ) R8_AINT, the truncated version of X.
!
  implicit none

  real ( kind = 8 ) r8_aint
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    value = - int ( abs ( x ) )
  else
    value =   int ( abs ( x ) )
  end if

  r8_aint = value

  return
end
function r8_asin ( s )

!*****************************************************************************80
!
!! R8_ASIN computes the arc sine function, with argument truncation.
!
!  Discussion:
!
!    If you call your system ASIN routine with an input argument that is
!    even slightly outside the range [-1.0, 1.0 ], you may get an unpleasant 
!    surprise (I did).
!
!    This routine simply truncates arguments outside the range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) S, the argument.
!
!  Output:
!
!    real ( kind = 8 ) R8_ASIN, an angle whose sine is S.
!
  implicit none

  real ( kind = 8 ) r8_asin
  real ( kind = 8 ) s
  real ( kind = 8 ) s2
  real ( kind = 8 ) value

  s2 = s
  s2 = max ( s2, -1.0D+00 )
  s2 = min ( s2, +1.0D+00 )

  value = asin ( s2 )

  r8_asin = value

  return
end
function r8_asinh ( x )

!*****************************************************************************80
!
!! R8_ASINH returns the inverse hyperbolic sine of a number.
!
!  Discussion:
!
!    The assertion that:
!
!      Y = R8_ASINH ( X )
!
!    implies that
!
!      X = SINH(Y) = 0.5 * ( EXP(Y) - EXP(-Y) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2007
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose inverse hyperbolic 
!    sine is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8_ASINH, the inverse hyperbolic sine of X.
!
  implicit none

  real ( kind = 8 ) r8_asinh
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  value = log ( x + sqrt ( x * x + 1.0D+00 ) )

  r8_asinh = value

  return
end
function r8_atan ( y, x )

!*****************************************************************************80
!
!! R8_ATAN computes the inverse tangent of the ratio Y / X.
!
!  Discussion:
!
!    R8_ATAN returns an angle whose tangent is ( Y / X ), a job which
!    the built in functions ATAN and ATAN2 already do.
!
!    However:
!
!    * R8_ATAN always returns a positive angle, between 0 and 2 PI,
!      while ATAN and ATAN2 return angles in the interval [-PI/2,+PI/2]
!      and [-PI,+PI] respectively;
!
!    * R8_ATAN accounts for the signs of X and Y, (as does ATAN2).  The ATAN
!     function by contrast always returns an angle in the first or fourth
!     quadrants.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) Y, X, two quantities which represent the
!    tangent of an angle.  If Y is not zero, then the tangent is (Y/X).
!
!  Output:
!
!    real ( kind = 8 ) R8_ATAN, an angle between 0 and 2 * PI, whose
!    tangent is (Y/X), and which lies in the appropriate quadrant so that
!    the signs of its cosine and sine match those of X and Y.
!
  implicit none

  real ( kind = 8 ) abs_x
  real ( kind = 8 ) abs_y
  real ( kind = 8 ) r8_atan
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) theta_0
  real ( kind = 8 ) value
  real ( kind = 8 ) x
  real ( kind = 8 ) y
!
!  Special cases:
!
  if ( x == 0.0D+00 ) then

    if ( 0.0D+00 < y ) then
      value = r8_pi / 2.0D+00
    else if ( y < 0.0D+00 ) then
      value = 3.0D+00 * r8_pi / 2.0D+00
    else if ( y == 0.0D+00 ) then
      value = 0.0D+00
    end if

  else if ( y == 0.0D+00 ) then

    if ( 0.0D+00 < x ) then
      value = 0.0D+00
    else if ( x < 0.0D+00 ) then
      value = r8_pi
    end if
!
!  We assume that ATAN2 is correct when both arguments are positive.
!
  else

    abs_y = abs ( y )
    abs_x = abs ( x )

    theta_0 = atan2 ( abs_y, abs_x )

    if ( 0.0D+00 < x .and. 0.0D+00 < y ) then
      value = theta_0
    else if ( x < 0.0D+00 .and. 0.0D+00 < y ) then
      value = r8_pi - theta_0
    else if ( x < 0.0D+00 .and. y < 0.0D+00 ) then
      value = r8_pi + theta_0
    else if ( 0.0D+00 < x .and. y < 0.0D+00 ) then
      value = 2.0D+00 * r8_pi - theta_0
    end if

  end if

  r8_atan = value

  return
end
function r8_atanh ( x )

!*****************************************************************************80
!
!! R8_ATANH returns the inverse hyperbolic tangent of a number.
!
!  Discussion:
!
!    Y = R8_ATANH ( X )
!
!    implies that
!
!    X = TANH(Y) = ( EXP(Y) - EXP(-Y) ) / ( EXP(Y) + EXP(-Y) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 November 2007
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose inverse hyperbolic 
!    tangent is desired.  The absolute value of X should be less than 
!    or equal to 1.
!
!  Output:
!
!    real ( kind = 8 ) R8_ATANH, the inverse hyperbolic tangent of X.
!
  implicit none

  real ( kind = 8 ) r8_atanh
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  if ( 1.0D+00 <= abs ( x ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_ATANH - Fatal error!'
    write ( *, '(a)' ) '  ABS(X) must be < 1.'
    write ( *, '(a,g14.6)' ) '  Your input is X = ', x
    stop 1
  end if

  value = 0.5D+00 * log ( ( 1.0D+00 + x ) / ( 1.0D+00 - x ) )

  r8_atanh = value

  return
end
function r8_big ( )

!*****************************************************************************80
!
!! R8_BIG returns a big R8.
!
!  Discussion:
!
!    The value returned by this function is NOT required to be the
!    maximum representable R8.
!    We simply want a "very large" but non-infinite number.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Output:
!
!    real ( kind = 8 ) R8_BIG, a "big" value.
!
  implicit none

  real ( kind = 8 ) r8_big
  real ( kind = 8 ), parameter :: value = 1.0D+30

  r8_big = value

  return
end
function r8_cas ( x )

!*****************************************************************************80
!
!! R8_CAS returns the "casine" of an R8.
!
!  Discussion:
!
!    The "casine", used in the discrete Hartley transform, is abbreviated
!    CAS(X), and defined by:
!
!      CAS(X) = cos ( X ) + sin( X )
!             = sqrt ( 2 ) * sin ( X + pi/4 )
!             = sqrt ( 2 ) * cos ( X - pi/4 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Ralph Hartley,
!    A More Symmetrical Fourier Analysis Applied to Transmission Problems,
!    Proceedings of the Institute of Radio Engineers,
!    Volume 30, pages 144-150, 1942.
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose casine is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8_CAS, the casine of X, which will be between
!    plus or minus the square root of 2.
!
  implicit none

  real ( kind = 8 ) r8_cas
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  value = cos ( x ) + sin ( x )

  r8_cas = value

  return
end
function r8_ceiling ( r )

!*****************************************************************************80
!
!! R8_CEILING rounds an R8 "up" (towards +oo) to an integral R8.
!
!  Example:
!
!    R     Value
!
!   -1.1  -1.0
!   -1.0  -1.0
!   -0.9   0.0
!    0.0   0.0
!    5.0   5.0
!    5.1   6.0
!    5.9   6.0
!    6.0   6.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, the value to be rounded up.
!
!  Output:
!
!    real ( kind = 8 ) R8_CEILING, the rounded value.
!
  implicit none

  real ( kind = 8 ) r
  integer ( kind = 4 ) r_int
  real ( kind = 8 ) r8_ceiling
  real ( kind = 8 ) value

  r_int = int ( r )
  value = real ( r_int, kind = 8 )
  if ( value < r ) then
    value = value + 1.0D+00
  end if

  r8_ceiling = value

  return
end
function r8_choose ( n, k )

!*****************************************************************************80
!
!! R8_CHOOSE computes the combinatorial coefficient C(N,K).
!
!  Discussion:
!
!    Real arithmetic is used, and C(N,K) is computed directly, via
!    Gamma functions, rather than recursively.
!
!    C(N,K) is the number of distinct combinations of K objects
!    chosen from a set of N distinct objects.  A combination is
!    like a set, in that order does not matter.
!
!    C(N,K) = N! / ( (N-K)! * K! )
!
!  Example:
!
!    The number of combinations of 2 things chosen from 5 is 10.
!
!    C(5,2) = ( 5 * 4 * 3 * 2 * 1 ) / ( ( 3 * 2 * 1 ) * ( 2 * 1 ) ) = 10.
!
!    The actual combinations may be represented as:
!
!      (1,2), (1,3), (1,4), (1,5), (2,3),
!      (2,4), (2,5), (3,4), (3,5), (4,5).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the value of N.
!
!    integer ( kind = 4 ) K, the value of K.
!
!  Output:
!
!    real ( kind = 8 ) R8_CHOOSE, the value of C(N,K)
!
  implicit none

  real ( kind = 8 ) arg
  real ( kind = 8 ) fack
  real ( kind = 8 ) facn
  real ( kind = 8 ) facnmk
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_choose
  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) value

  if ( n < 0 ) then

    value = 0.0D+00

  else if ( k == 0 ) then

    value = 1.0D+00

  else if ( k == 1 ) then

    value = real ( n, kind = 8 )

  else if ( 1 < k .and. k < n - 1 ) then

    arg = real ( n + 1, kind = 8 )
    facn = r8_gamma_log ( arg )

    arg = real ( k + 1, kind = 8 )
    fack = r8_gamma_log ( arg )

    arg = real ( n - k + 1, kind = 8 )
    facnmk = r8_gamma_log ( arg )

    value = real ( nint ( exp ( facn - fack - facnmk ) ),  kind = 8 )

  else if ( k == n - 1 ) then

    value = real ( n, kind = 8 )

  else if ( k == n ) then

    value = 1.0D+00

  else

    value = 0.0D+00

  end if

  r8_choose = value

  return
end
function r8_chop ( place, x )

!*****************************************************************************80
!
!! R8_CHOP chops an R8 to a given number of binary places.
!
!  Example:
!
!    3.875 = 2 + 1 + 1/2 + 1/4 + 1/8.
!
!    The following values would be returned for the 'chopped' value of
!    3.875:
!
!    PLACE  Value
!
!       1      2
!       2      3     = 2 + 1
!       3      3.5   = 2 + 1 + 1/2
!       4      3.75  = 2 + 1 + 1/2 + 1/4
!       5+     3.875 = 2 + 1 + 1/2 + 1/4 + 1/8
!
!    PLACE = 0 means return the integer part of X.
!    PLACE = 1 means return the value of X, correct to 1/2.
!    PLACE = 2 means return the value of X, correct to 1/4.
!    PLACE = -1 means return the value of X, correct to 2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) PLACE, the number of binary places to preserve.
!    PLACE < 32.
!
!    real ( kind = 8 ) X, the number to be chopped.
!
!  Output:
!
!    real ( kind = 8 ) R8_CHOP, the chopped number.
!
  implicit none

  real ( kind = 8 ) fac
  integer ( kind = 4 ) place
  real ( kind = 8 ) r8_chop
  real ( kind = 8 ) r8_log_2
  real ( kind = 8 ) r8_sign
  real ( kind = 8 ) s
  integer ( kind = 4 ) temp
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  s = r8_sign ( x )
  temp = int ( r8_log_2 ( abs ( x ) ) )
  fac = 2.0D+00 ** ( temp - place + 1 )
  value = s * real ( int ( abs ( x ) / fac ), kind = 8 ) * fac

  r8_chop = value

  return
end
function r8_cosd ( degrees )

!*****************************************************************************80
!
!! R8_COSD returns the cosine of an angle given in degrees.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) DEGREES, the angle in degrees.
!
!  Output:
!
!    real ( kind = 8 ) R8_COSD, the cosine of the angle.
!
  implicit none

  real ( kind = 8 ) degrees
  real ( kind = 8 ) r8_cosd
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) radians
  real ( kind = 8 ) value

  radians = r8_pi * ( degrees / 180.0D+00 )
  value = cos ( radians )

  r8_cosd = value

  return
end
function r8_cot ( angle )

!*****************************************************************************80
!
!! R8_COT returns the cotangent of an angle.
!
!  Discussion:
!
!    R8_COT ( THETA ) = COS ( THETA ) / SIN ( THETA )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) ANGLE, the angle, in radians.
!
!  Output:
!
!    real ( kind = 8 ) R8_COT, the cotangent of the angle.
!
  implicit none

  real ( kind = 8 ) angle
  real ( kind = 8 ) r8_cot
  real ( kind = 8 ) value

  value = cos ( angle ) / sin ( angle )

  r8_cot = value

  return
end
function r8_cotd ( degrees )

!*****************************************************************************80
!
!! R8_COTD returns the cotangent of an angle given in degrees.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) DEGREES, the angle in degrees.
!
!  Output:
!
!    real ( kind = 8 ) R8_COTD, the cotangent of the angle.
!
  implicit none

  real ( kind = 8 ) degrees
  real ( kind = 8 ) r8_cotd
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) radians
  real ( kind = 8 ) value

  radians = r8_pi * ( degrees / 180.0D+00 )
  value = cos ( radians ) / sin ( radians )

  r8_cotd = value

  return
end
function r8_csc ( theta )

!*****************************************************************************80
!
!! R8_CSC returns the cosecant of X.
!
!  Discussion:
!
!    R8_CSC ( THETA ) = 1.0 / SIN ( THETA )
!
!    The cosecant is not a built-in function in FORTRAN, and occasionally it
!    is handier, or more concise, to be able to refer to it directly
!    rather than through its definition in terms of the sine function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) THETA, the angle, in radians, whose
!    cosecant is desired.  It must be the case that SIN ( THETA ) is not zero.
!
!  Output:
!
!    real ( kind = 8 ) R8_CSC, the cosecant of THETA.
!
  implicit none

  real ( kind = 8 ) r8_csc
  real ( kind = 8 ) theta
  real ( kind = 8 ) value

  value = sin ( theta )

  if ( value == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_CSC - Fatal error!'
    write ( *, '(a,g14.6)' ) '  Cosecant undefined for THETA = ', theta
    stop 1
  end if

  r8_csc = 1.0D+00 / value

  return
end
function r8_cscd ( degrees )

!*****************************************************************************80
!
!! R8_CSCD returns the cosecant of an angle given in degrees.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) DEGREES, the angle in degrees.
!
!  Output:
!
!    real ( kind = 8 ) R8_CSCD, the cosecant of the angle.
!
  implicit none

  real ( kind = 8 ) degrees
  real ( kind = 8 ) r8_cscd
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) radians

  radians = r8_pi * ( degrees / 180.0D+00 )
  r8_cscd  = 1.0D+00 / sin ( radians )

  return
end
function r8_csqrt ( x )

!*****************************************************************************80
!
!! R8_CSQRT returns the complex square root of an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose square root is desired.
!
!  Output:
!
!    complex ( kind = 8 ) R8_CSQRT, the square root of X:
!
  implicit none

  real ( kind = 8 ) argument
  real ( kind = 8 ) magnitude
  complex ( kind = 8 ) r8_csqrt
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  if ( 0.0D+00 < x ) then
    magnitude = x
    argument = 0.0D+00
  else if ( 0.0D+00 == x ) then
    magnitude = 0.0D+00
    argument = 0.0D+00
  else if ( x < 0.0D+00 ) then
    magnitude = -x
    argument = r8_pi
  end if

  magnitude = sqrt ( magnitude )
  argument = argument / 2.0D+00

  r8_csqrt = magnitude * cmplx ( cos ( argument ), sin ( argument ), kind = 8 )

  return
end
function r8_cube_root ( x )

!*****************************************************************************80
!
!! R8_CUBE_ROOT returns the cube root of an R8.
!
!  Discussion:
!
!    This routine is designed to avoid the possible problems that can occur
!    when formulas like 0.0^(1/3) or (-1.0)^(1/3) are to be evaluated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose cube root is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8_CUBE_ROOT, the cube root of X.
!
  implicit none

  real ( kind = 8 ) r8_cube_root
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  if ( 0.0D+00 < x ) then
    value = x ** ( 1.0D+00 / 3.0D+00 )
  else if ( x == 0.0D+00 ) then
    value = 0.0D+00
  else
    value = -( abs ( x ) ) ** ( 1.0D+00 / 3.0D+00 )
  end if

  r8_cube_root = value

  return
end
function r8_degrees ( radians )

!*****************************************************************************80
!
!! R8_DEGREES converts an angle from radian to degree measure.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) RADIANS, the angle measurement in radians.
!
!  Output:
!
!    real ( kind = 8 ) R8_DEGREES, the angle measurement in degrees.
!
  implicit none

  real ( kind = 8 ) r8_degrees
  real ( kind = 8 ), parameter :: r8_pi = 3.1415926535897932384626434D+00
  real ( kind = 8 ) radians

  r8_degrees = radians * 180.0D+00 / r8_pi

  return
end
function r8_delta ( i, j )

!*****************************************************************************80
!
!! r8_delta evaluates the delta function on a pair of integers.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) I, J: the values to compare.
!
!  Output:
!
!    real ( kind = 8 ) R8_DELTA: 1 if I=J, 0 otherwise.
!
  implicit none

  real ( kind = 8 ) r8_delta
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  if ( i == j ) then
    r8_delta = 1.0D+00
  else
    r8_delta = 0.0D+00
  end if

  return
end
function r8_diff ( x, y, n )

!*****************************************************************************80
!
!! R8_DIFF computes the difference of two R8's to a specified accuracy.
!
!  Discussion:
!
!    The user controls how many binary digits of accuracy
!    are to be used.
!
!    N determines the accuracy of the value of the result.  If N = 10,
!    for example, only 11 binary places will be used in the arithmetic.
!    In general, only N+1 binary places will be used.
!
!    N may be zero.  However, a negative value of N should
!    not be used, since this will cause both X and Y to look like 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 November 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, Y, the two values whose difference is desired.
!
!    integer ( kind = 4 ) N, the number of binary digits to use.
!
!  Output:
!
!    real ( kind = 8 ) R8_DIFF, the value of X-Y.
!
  implicit none

  real ( kind = 8 ) cx
  real ( kind = 8 ) cy
  integer ( kind = 4 ) n
  real ( kind = 8 ) pow2
  real ( kind = 8 ) r8_diff
  real ( kind = 8 ) size
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x == y ) then
    r8_diff = 0.0D+00
    return
  end if

  pow2 = 2.0D+00 ** n
!
!  Compute the magnitude of X and Y, and take the larger of the
!  two.  At least one of the two values is not zero!
!
  size = max ( abs ( x ), abs ( y ) )
!
!  Make normalized copies of X and Y.  One of the two values will
!  actually be equal to 1.
!
  cx = x / size
  cy = y / size
!
!  Here's where rounding comes in.  We know that the larger of the
!  the two values equals 1.  We multiply both values by 2^N,
!  where N+1 is the number of binary digits of accuracy we want
!  to use, truncate the values, and divide back by 2^N.
!
  cx = real ( int ( cx * pow2 + sign ( 0.5D+00, cx ) ), kind = 8 ) / pow2
  cy = real ( int ( cy * pow2 + sign ( 0.5D+00, cy ) ), kind = 8 ) / pow2
!
!  Take the difference now.
!
  r8_diff = cx - cy
!
!  Undo the scaling.
!
  r8_diff = r8_diff * size

  return
end
subroutine r8_digit ( x, idigit, digit )

!*****************************************************************************80
!
!! R8_DIGIT returns a particular decimal digit of an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 November 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose NDIG-th decimal digit
!    is desired.  If X is zero, all digits will be returned as 0.
!
!    integer ( kind = 4 ) IDIGIT, the position of the desired decimal
!    digit.  A value of 1 means the leading digit, a value of 2 the second digit
!    and so on.
!
!  Output:
!
!    integer ( kind = 4 ) DIGIT, the value of the IDIGIT-th decimal
!    digit of X.
!
  implicit none

  integer ( kind = 4 ) digit
  integer ( kind = 4 ) i
  integer ( kind = 4 ) idigit
  integer ( kind = 4 ) ival
  real ( kind = 8 ) x
  real ( kind = 8 ) xcopy

  if ( x == 0.0D+00 ) then
    digit = 0
    return
  end if

  if ( idigit <= 0 ) then
    digit = 0
    return
  end if
!
!  Set XCOPY = X, and then force XCOPY to lie between 1 and 10.
!
  xcopy = abs ( x )

  do while ( xcopy < 1.0D+00 )
    xcopy = xcopy * 10.0D+00
  end do

  do while ( 10.0D+00 <= xcopy )
    xcopy = xcopy / 10.0D+00
  end do

  do i = 1, idigit
    ival = int ( xcopy )
    xcopy = ( xcopy - ival ) * 10.0D+00
  end do

  digit = ival

  return
end
function r8_divide_i4 ( i, j )

!*****************************************************************************80
!
!! R8_DIVIDE_I4 returns an I4 fraction as an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 June 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) I, J, the numerator and denominator.
!
!  Output:
!
!    real ( kind = 8 ) R8_DIVIDE_I4, the value of (I/J).
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_divide_i4

  r8_divide_i4 = real ( i, kind = 8 ) / real ( j, kind = 8 )

  return
end
function r8_e ( )

!*****************************************************************************80
!
!! R8_E returns the value of the base of the natural logarithm system.
!
!  Discussion:
!
!    E = Limit ( N -> oo ) ( 1 + 1 / N )^N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Output:
!
!    real ( kind = 8 ) R8_E, the base of the natural 
!    logarithm system.
!
  implicit none

  real ( kind = 8 ) r8_e

  r8_e = 2.718281828459045235360287D+00
 
  return
end
function r8_epsilon ( )

!*****************************************************************************80
!
!! R8_EPSILON returns the R8 roundoff unit.
!
!  Discussion:
!
!    The roundoff unit is a number R which is a power of 2 with the
!    property that, to the precision of the computer's arithmetic,
!      1 < 1 + R
!    but
!      1 = ( 1 + R / 2 )
!
!    FORTRAN90 provides the superior library routine
!
!      EPSILON ( X )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Output:
!
!    real ( kind = 8 ) R8_EPSILON, the round-off unit.
!
  implicit none

  real ( kind = 8 ) r8_epsilon
  real ( kind = 8 ), parameter :: t = 1.0D+00

  r8_epsilon = epsilon ( t )

  return
end
function r8_epsilon_compute ( )

!*****************************************************************************80
!
!! R8_EPSILON_COMPUTE computes the R8 roundoff unit.
!
!  Discussion:
!
!    The roundoff unit is a number R which is a power of 2 with the
!    property that, to the precision of the computer's arithmetic,
!      1 < 1 + R
!    but
!      1 = ( 1 + R / 2 )
!
!    FORTRAN90 provides the superior library routine
!
!      EPSILON ( X )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2012
!
!  Author:
!
!    John Burkardt
!
!  Output:
!
!    real ( kind = 8 ) R8_EPSILON_COMPUTE, the computed round-off unit.
!
  implicit none

  real ( kind = 8 ) one
  real ( kind = 8 ) r8_add
  real ( kind = 8 ) r8_epsilon_compute
  real ( kind = 8 ) temp
  real ( kind = 8 ) test
  real ( kind = 8 ) value

  one = real ( 1, kind = 8 )

  value = one
  temp = value / 2.0D+00
  test = r8_add ( one, temp )

  do while ( one < test )
    value = temp
    temp = value / 2.0D+00
    test = r8_add ( one, temp )
  end do

  r8_epsilon_compute = value

  return
end
function r8_exp ( x )

!*****************************************************************************80
!
!! R8_EXP computes the exponential of an R8, avoiding overflow and underflow.
!
!  Discussion:
!
!    For arguments of very large magnitude, the evaluation of the
!    exponential function can cause computational problems.  Some languages
!    and compilers may return an infinite value or a "Not-a-Number".  
!    An alternative, when dealing with a wide range of inputs, is simply
!    to truncate the calculation for arguments whose magnitude is too large.
!    Whether this is the right or convenient approach depends on the problem
!    you are dealing with, and whether or not you really need accurate
!    results for large magnitude inputs, or you just want your code to
!    stop crashing.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the argument of the exponential function.
!
!  Output:
!
!    real ( kind = 8 ) R8_EXP, the value of exp ( X ).
!
  implicit none

  real ( kind = 8 ) r8_exp
  real ( kind = 8 ), parameter :: r8_huge = 1.79769313486231571D+308
  real ( kind = 8 ), parameter :: r8_log_max = +69.0776D+00
  real ( kind = 8 ), parameter :: r8_log_min = -69.0776D+00
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  if ( x <= r8_log_min ) then
    value = 0.0D+00
  else if ( x < r8_log_max ) then
    value = exp ( x )
  else
    value = r8_huge
  end if

  r8_exp = value

  return
end
function r8_factorial ( n )

!*****************************************************************************80
!
!! R8_FACTORIAL computes the factorial of N.
!
!  Discussion:
!
!    factorial ( N ) = product ( 1 <= I <= N ) I
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the argument of the factorial function.
!    If N is less than 1, the function value is returned as 1.
!
!  Output:
!
!    real ( kind = 8 ) R8_FACTORIAL, the factorial of N.
!
  implicit none

  real ( kind = 8 ) r8_factorial
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  value = 1.0D+00

  do i = 1, n
    value = value * real ( i, kind = 8 )
  end do

  r8_factorial = value

  return
end
function r8_factorial_stirling ( n )

!*****************************************************************************80
!
!! R8_FACTORIAL_STIRLING computes Stirling's approximation to N!.
!
!  Discussion:
!
!    This routine returns the raw approximation for all nonnegative
!    values of N.  If N is less than 0, the value is returned as 0,
!    and if N is 0, the value of 1 is returned.  In all other cases,
!    Stirling's formula is used.
!
!    The factorial function N! is defined by
!
!      N! = Product ( 1 <= I <= N ) I
!
!    Stirling's approximation to N! is
!
!      Stirling ( N ) = sqrt ( 2 * PI * N ) * ( N / E )^N * E^(1/(12*N) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the argument of the function.
!
!  Output:
!
!    real ( kind = 8 ) R8_FACTORIAL_STIRLING, an approximation to N!.
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: r8_e = 2.71828182845904523D+00
  real ( kind = 8 ) r8_factorial_stirling
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) value

  if ( n < 0 ) then

    value = 0.0D+00

  else if ( n == 0 ) then

    value = 1.0D+00

  else

    value = sqrt ( 2.0D+00 * r8_pi * real ( n, kind = 8 ) ) &
      * ( real ( n, kind = 8 ) / r8_e ) ** n &
      * exp ( 1.0D+00 / real ( 12 * n, kind = 8 ) )

  end if

  r8_factorial_stirling = value

  return
end
subroutine r8_factorial_values ( n_data, n, fn )

!*****************************************************************************80
!
!! R8_FACTORIAL_VALUES returns values of the real factorial function.
!
!  Discussion:
!
!    0! = 1
!    I! = Product ( 1 <= J <= I ) J
!
!    Although the factorial is an integer valued function, it quickly
!    becomes too large for an integer to hold.  This routine still accepts
!    an integer as the input argument, but returns the function value
!    as a real number.
!
!    In Mathematica, the function can be evaluated by:
!
!      n!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 August 2004
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 before 
!    the first call.  
!
!  Output:
!
!    integer ( kind = 4 ) N_DATA.  On each call, the routine increments 
!    N_DATA by 1, and returns the corresponding data; when there is no 
!    more data, the output value of N_DATA will be 0 again.
!
!    integer ( kind = 4 ) N, the argument of the function.
!
!    real ( kind = 8 ) FN, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 25

  real ( kind = 8 ) fn
  real ( kind = 8 ), save, dimension ( n_max ) :: fn_vec = (/ &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.2000000000000000D+01, &
    0.6000000000000000D+01, &
    0.2400000000000000D+02, &
    0.1200000000000000D+03, &
    0.7200000000000000D+03, &
    0.5040000000000000D+04, &
    0.4032000000000000D+05, &
    0.3628800000000000D+06, &
    0.3628800000000000D+07, &
    0.3991680000000000D+08, &
    0.4790016000000000D+09, &
    0.6227020800000000D+10, &
    0.8717829120000000D+11, &
    0.1307674368000000D+13, &
    0.2092278988800000D+14, &
    0.3556874280960000D+15, &
    0.6402373705728000D+16, &
    0.1216451004088320D+18, &
    0.2432902008176640D+19, &
    0.1551121004333099D+26, &
    0.3041409320171338D+65, &
    0.9332621544394415D+158, &
    0.5713383956445855D+263 /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ), save, dimension ( n_max ) :: n_vec = (/ &
       0, &
       1, &
       2, &
       3, &
       4, &
       5, &
       6, &
       7, &
       8, &
       9, &
      10, &
      11, &
      12, &
      13, &
      14, &
      15, &
      16, &
      17, &
      18, &
      19, &
      20, &
      25, &
      50, &
     100, &
     150 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    fn = 0.0D+00
  else
    n = n_vec(n_data)
    fn = fn_vec(n_data)
  end if

  return
end
function r8_factorial2 ( n )

!*****************************************************************************80
!
!! R8_FACTORIAL2 computes the double factorial function.
!
!  Discussion:
!
!    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
!                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
!
!  Example:
!
!     N Value
!
!     0     1
!     1     1
!     2     2
!     3     3
!     4     8
!     5    15
!     6    48
!     7   105
!     8   384
!     9   945
!    10  3840
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2007
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the argument of the double factorial
!    function.  If N is less than 1, the value is returned as 1.0.
!
!  Output:
!
!    real ( kind = 8 ) R8_FACTORIAL2, the value.
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_factorial2
  real ( kind = 8 ) r8_n
  real ( kind = 8 ) value

  value = 1.0D+00

  if ( 1 <= n ) then

    r8_n = real ( n, kind = 8 )

    do while ( 1.0D+00 < r8_n )
      value = value * r8_n
      r8_n = r8_n - 2.0D+00
    end do

  end if

  r8_factorial2 = value

  return
end
subroutine r8_factorial2_values ( n_data, n, f )

!*****************************************************************************80
!
!! R8_FACTORIAL2_VALUES returns values of the double factorial function.
!
!  Discussion:
!
!    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
!                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
!
!    In Mathematica, the function can be evaluated by:
!
!      n!!
!
!  Example:
!
!     N    N!!
!
!     0     1
!     1     1
!     2     2
!     3     3
!     4     8
!     5    15
!     6    48
!     7   105
!     8   384
!     9   945
!    10  3840
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 August 2004
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996,
!    ISBN: 0-8493-2479-3,
!    LC: QA47.M315.
!
!  Input:
!
!    integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 before 
!    the first call.  
!
!  Output:
!
!    integer ( kind = 4 ) N_DATA.  On each call, the routine increments 
!    N_DATA by 1, and returns the corresponding data; when there is no 
!    more data, the output value of N_DATA will be 0 again.
!
!    integer ( kind = 4 ) N, the argument of the function.
!
!    real ( kind = 8 ) F, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 16

  real ( kind = 8 ), save, dimension ( n_max ) :: f_vec = (/ &
          1.0D+00, &
          1.0D+00, &
          2.0D+00, &
          3.0D+00, &
          8.0D+00, &
         15.0D+00, &
         48.0D+00, &
        105.0D+00, &
        384.0D+00, &
        945.0D+00, &
       3840.0D+00, &
      10395.0D+00, &
      46080.0D+00, &
     135135.0D+00, &
     645120.0D+00, &
    2027025.0D+00 /)
  real ( kind = 8 ) f
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ) n
  integer ( kind = 4 ), save, dimension ( n_max ) :: n_vec = (/ &
     0, &
     1,  2,  3,  4,  5, &
     6,  7,  8,  9, 10, &
    11, 12, 13, 14, 15 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    n = 0
    f = 0.0D+00
  else
    n = n_vec(n_data)
    f = f_vec(n_data)
  end if

  return
end
subroutine r8_fake_use ( x )

!*****************************************************************************80
!
!! r8_fake_use pretends to use a variable.
!
!  Discussion:
!
!    Some compilers will issue a warning if a variable is unused.
!    Sometimes there's a good reason to include a variable in a program,
!    but not to use it.  Calling this function with that variable as
!    the argument will shut the compiler up.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the variable to be "used".
!
  implicit none

  real ( kind = 8 ) x

  if ( x /= x ) then
    write ( *, '(a)' ) '  r8_fake_use: variable is NAN.'
  end if

  return
end
function r8_fall ( x, n )

!*****************************************************************************80
!
!! R8_FALL computes the falling factorial function [X]_N.
!
!  Discussion:
!
!    Note that the number of "injections" or 1-to-1 mappings from
!    a set of N elements to a set of M elements is [M]_N.
!
!    The number of permutations of N objects out of M is [M]_N.
!
!    Moreover, the Stirling numbers of the first kind can be used
!    to convert a falling factorial into a polynomial, as follows:
!
!      [X]_N = S^0_N + S^1_N * X + S^2_N * X^2 + ... + S^N_N X^N.
!
!    The formula used is:
!
!      [X]_N = X * ( X - 1 ) * ( X - 2 ) * ... * ( X - N + 1 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the argument of the falling factorial function.
!
!    integer ( kind = 4 ) N, the order of the falling factorial function.
!    If N = 0, FALL = 1, if N = 1, FALL = X.  Note that if N is
!    negative, a "rising" factorial will be computed.
!
!  Output:
!
!    real ( kind = 8 ) R8_FALL, the value of the falling
!    factorial function.
!
  implicit none

  real ( kind = 8 ) arg
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_fall
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  value = 1.0D+00

  arg = x

  if ( 0 < n ) then

    do i = 1, n
      value = value * arg
      arg = arg - 1.0D+00
    end do

  else if ( n < 0 ) then

    do i = -1, n, -1
      value = value * arg
      arg = arg + 1.0D+00
    end do

  end if

  r8_fall = value

  return
end
subroutine r8_fall_values ( n_data, x, n, f )

!*****************************************************************************80
!
!! R8_FALL_VALUES returns some values of the falling factorial function.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      FactorialPower[X,Y]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 December 2014
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 before 
!    the first call.  
!
!  Output:
!
!    integer ( kind = 4 ) N_DATA.  On each call, the routine increments 
!    N_DATA by 1, and returns the corresponding data; when there is no 
!    more data, the output value of N_DATA will be 0 again.
!
!    real ( kind = 8 ) X, integer ( kind = 4 ) N, the arguments.
!
!    real ( kind = 8 ) F, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 15

  real ( kind = 8 ), save, dimension ( n_max ) :: f_vec = (/ &
       120.0000000000000D+00,  &
        163.1601562500000D+00, &
        216.5625000000000D+00, &
        281.6601562500000D+00, &
        360.0000000000000D+00, &
        1.000000000000000D+00, &
        7.500000000000000D+00, &
        48.75000000000000D+00, &
        268.1250000000000D+00, &
        1206.562500000000D+00, &
        4222.968750000000D+00, &
        10557.42187500000D+00, &
        15836.13281250000D+00, &
        7918.066406250000D+00, &
        -3959.03320312500D+00 /)
  real ( kind = 8 ) f
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ), save, dimension ( n_max ) :: n_vec = (/ &
        4, &
        4, &
        4, &
        4, &
        4, &
        0, &
        1, &
        2, &
        3, &
        4, &
        5, &
        6, &
        7, &
        8, &
        9 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
        5.00D+00, &
        5.25D+00, &
        5.50D+00, &
        5.75D+00, &
        6.00D+00, &
        7.50D+00, &
        7.50D+00, &
        7.50D+00, &
        7.50D+00, &
        7.50D+00, &
        7.50D+00, &
        7.50D+00, &
        7.50D+00, &
        7.50D+00, &
        7.50D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    n = 0
    f = 0.0D+00
  else
    x = x_vec(n_data)
    n = n_vec(n_data)
    f = f_vec(n_data)
  end if

  return
end
function r8_floor ( r )

!*****************************************************************************80
!
!! R8_FLOOR rounds an R8 "down" (towards -oo) to the nearest integral R8.
!
!  Example:
!
!    R     Value
!
!   -1.1  -2.0
!   -1.0  -1.0
!   -0.9  -1.0
!    0.0   0.0
!    5.0   5.0
!    5.1   5.0
!    5.9   5.0
!    6.0   6.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, the value to be rounded down.
!
!  Output:
!
!    real ( kind = 8 ) R8_FLOOR, the rounded value.
!
  implicit none

  real ( kind = 8 ) r
  real ( kind = 8 ) r8_floor
  real ( kind = 8 ) value

  value = real ( int ( r ), kind = 8 )
  if ( r < value ) then
    value = value - 1.0D+00
  end if

  r8_floor = value

  return
end
function r8_fraction ( i, j )

!*****************************************************************************80
!
!! R8_FRACTION uses real arithmetic on an integer ratio.
!
!  Discussion:
!
!    Given integer variables I and J, both FORTRAN and C will evaluate
!    an expression such as "I/J" using what is called "integer division",
!    with the result being an integer.  It is often convenient to express
!    the parts of a fraction as integers but expect the result to be computed
!    using real arithmetic.  This function carries out that operation.
!
!  Example:
!
!       I     J   I/J  R8_FRACTION
!
!       1     2     0  0.5
!       7     4     1  1.75
!       8     4     2  2.00
!       9     4     2  2.25
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) I, J, the arguments.
!
!  Output:
!
!    real ( kind = 8 ) R8_FRACTION, the value of the ratio.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8_fraction

  r8_fraction = real ( i, kind = 8 ) / real ( j, kind = 8 )

  return
end
function r8_fractional ( x )

!*****************************************************************************80
!
!! R8_FRACTIONAL returns the fractional part of an R8.
!
!  Discussion:
!
!    If we regard a real number as
!
!      R = SIGN * ( WHOLE + FRACTION )
!
!    where
!
!      SIGN is +1 or -1,
!      WHOLE is a nonnegative integer
!      FRACTION is a nonnegative real number strictly less than 1,
!
!    then this routine returns the value of FRACTION.
!
!  Example:
!
!     R      FRACTION
!
!    0.00      0.00
!    1.01      0.01
!    2.02      0.02
!   19.73      0.73
!   -4.34      0.34
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the argument.
!
!  Output:
!
!    real ( kind = 8 ) R8_FRACTIONAL, the fractional part of X.
!
  implicit none

  real ( kind = 8 ) r8_fractional
  real ( kind = 8 ) x

  r8_fractional = abs ( x ) - real ( int ( abs ( x ) ), kind = 8 )

  return
end
function r8_gamma ( x )

!*****************************************************************************80
!
!! R8_GAMMA evaluates Gamma(X) for a real argument.
!
!  Discussion:
!
!    This routine calculates the gamma function for a real argument X.
!
!    Computation is based on an algorithm outlined in reference 1.
!    The program uses rational functions that approximate the gamma
!    function to at least 20 significant decimal digits.  Coefficients
!    for the approximation over the interval (1,2) are unpublished.
!    Those for the approximation for 12 <= X are from reference 2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody,
!    An Overview of Software Development for Special Functions,
!    in Numerical Analysis Dundee, 1975,
!    edited by GA Watson,
!    Lecture Notes in Mathematics 506,
!    Springer, 1976.
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thatcher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Input:
!
!    real ( kind = 8 ) X, the argument of the function.
!
!  Output:
!
!    real ( kind = 8 ) R8_GAMMA, the value of the function.
!
  implicit none

  real ( kind = 8 ), dimension ( 7 ) :: c = (/ &
   -1.910444077728D-03, &
    8.4171387781295D-04, &
   -5.952379913043012D-04, &
    7.93650793500350248D-04, &
   -2.777777777777681622553D-03, &
    8.333333333333333331554247D-02, &
    5.7083835261D-03 /)
  real ( kind = 8 ) fact
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ), dimension ( 8 ) :: p = (/ &
    -1.71618513886549492533811D+00, &
     2.47656508055759199108314D+01, &
    -3.79804256470945635097577D+02, &
     6.29331155312818442661052D+02, &
     8.66966202790413211295064D+02, &
    -3.14512729688483675254357D+04, &
    -3.61444134186911729807069D+04, &
     6.64561438202405440627855D+04 /)
  logical ( kind = 4 ) parity
  real ( kind = 8 ), dimension ( 8 ) :: q = (/ &
    -3.08402300119738975254353D+01, &
     3.15350626979604161529144D+02, &
    -1.01515636749021914166146D+03, &
    -3.10777167157231109440444D+03, &
     2.25381184209801510330112D+04, &
     4.75584627752788110767815D+03, &
    -1.34659959864969306392456D+05, &
    -1.15132259675553483497211D+05 /)
  real ( kind = 8 ) r8_epsilon
  real ( kind = 8 ) r8_gamma
  real ( kind = 8 ), parameter :: r8_pi = 3.1415926535897932384626434D+00
  real ( kind = 8 ) res
  real ( kind = 8 ), parameter :: sqrtpi = 0.9189385332046727417803297D+00
  real ( kind = 8 ) sum
  real ( kind = 8 ) x
  real ( kind = 8 ), parameter :: xbig = 171.624D+00
  real ( kind = 8 ) xden
  real ( kind = 8 ), parameter :: xinf = 1.79D+308
  real ( kind = 8 ), parameter :: xminin = 2.23D-308
  real ( kind = 8 ) xnum
  real ( kind = 8 ) y
  real ( kind = 8 ) y1
  real ( kind = 8 ) ysq
  real ( kind = 8 ) z

  parity = .false.
  fact = 1.0D+00
  n = 0
  y = x
!
!  Argument is negative.
!
  if ( y <= 0.0D+00 ) then

    y = - x
    y1 = aint ( y )
    res = y - y1

    if ( res /= 0.0D+00 ) then

      if ( y1 /= aint ( y1 * 0.5D+00 ) * 2.0D+00 ) then
        parity = .true.
      end if

      fact = - r8_pi / sin ( r8_pi * res )
      y = y + 1.0D+00

    else

      res = xinf
      r8_gamma = res
      return

    end if

  end if
!
!  Argument is positive.
!
  if ( y < r8_epsilon ( ) ) then
!
!  Argument < EPS.
!
    if ( xminin <= y ) then
      res = 1.0D+00 / y
    else
      res = xinf
      r8_gamma = res
      return
    end if

  else if ( y < 12.0D+00 ) then

    y1 = y
!
!  0.0 < argument < 1.0.
!
    if ( y < 1.0D+00 ) then

      z = y
      y = y + 1.0D+00
!
!  1.0 < argument < 12.0.
!  Reduce argument if necessary.
!
    else

      n = int ( y ) - 1
      y = y - real ( n, kind = 8 )
      z = y - 1.0D+00

    end if
!
!  Evaluate approximation for 1.0 < argument < 2.0.
!
    xnum = 0.0D+00
    xden = 1.0D+00
    do i = 1, 8
      xnum = ( xnum + p(i) ) * z
      xden = xden * z + q(i)
    end do

    res = xnum / xden + 1.0D+00
!
!  Adjust result for case  0.0 < argument < 1.0.
!
    if ( y1 < y ) then

      res = res / y1
!
!  Adjust result for case 2.0 < argument < 12.0.
!
    else if ( y < y1 ) then

      do i = 1, n
        res = res * y
        y = y + 1.0D+00
      end do

    end if

  else
!
!  Evaluate for 12.0 <= argument.
!
    if ( y <= xbig ) then

      ysq = y * y
      sum = c(7)
      do i = 1, 6
        sum = sum / ysq + c(i)
      end do
      sum = sum / y - y + sqrtpi
      sum = sum + ( y - 0.5D+00 ) * log ( y )
      res = exp ( sum )

    else

      res = xinf
      r8_gamma = res
      return

    end if

  end if
!
!  Final adjustments and return.
!
  if ( parity ) then
    res = - res
  end if

  if ( fact /= 1.0D+00 ) then
    res = fact / res
  end if

  r8_gamma = res

  return
end
function r8_gamma_log ( x )

!*****************************************************************************80
!
!! R8_GAMMA_LOG evaluates the logarithm of the gamma function.
!
!  Discussion:
!
!    This routine calculates the LOG(GAMMA) function for a positive real
!    argument X.  Computation is based on an algorithm outlined in
!    references 1 and 2.  The program uses rational functions that
!    theoretically approximate LOG(GAMMA) to at least 18 significant
!    decimal digits.  The approximation for X > 12 is from reference
!    3, while approximations for X < 12.0 are similar to those in
!    reference 1, but are unpublished.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody, Kenneth Hillstrom,
!    Chebyshev Approximations for the Natural Logarithm of the
!    Gamma Function,
!    Mathematics of Computation,
!    Volume 21, Number 98, April 1967, pages 198-203.
!
!    Kenneth Hillstrom,
!    ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
!    May 1969.
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thatcher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Input:
!
!    real ( kind = 8 ) X, the argument of the function.
!
!  Output:
!
!    real ( kind = 8 ) R8_GAMMA_LOG, the value of the function.
!
  implicit none

  real ( kind = 8 ), dimension ( 7 ) :: c = (/ &
    -1.910444077728D-03, &
     8.4171387781295D-04, &
    -5.952379913043012D-04, &
     7.93650793500350248D-04, &
    -2.777777777777681622553D-03, &
     8.333333333333333331554247D-02, &
     5.7083835261D-03 /)
  real ( kind = 8 ) corr
  real ( kind = 8 ) :: d1 = -5.772156649015328605195174D-01
  real ( kind = 8 ) :: d2 = 4.227843350984671393993777D-01
  real ( kind = 8 ) :: d4 = 1.791759469228055000094023D+00
  real ( kind = 8 ), parameter :: frtbig = 2.25D+76
  integer ( kind = 4 ) i
  real ( kind = 8 ), dimension ( 8 ) :: p1 = (/ &
    4.945235359296727046734888D+00, &
    2.018112620856775083915565D+02, &
    2.290838373831346393026739D+03, &
    1.131967205903380828685045D+04, &
    2.855724635671635335736389D+04, &
    3.848496228443793359990269D+04, &
    2.637748787624195437963534D+04, &
    7.225813979700288197698961D+03 /)
  real ( kind = 8 ), dimension ( 8 ) :: p2 = (/ &
    4.974607845568932035012064D+00, &
    5.424138599891070494101986D+02, &
    1.550693864978364947665077D+04, &
    1.847932904445632425417223D+05, &
    1.088204769468828767498470D+06, &
    3.338152967987029735917223D+06, &
    5.106661678927352456275255D+06, &
    3.074109054850539556250927D+06 /)
  real ( kind = 8 ), dimension ( 8 ) :: p4 = (/ &
    1.474502166059939948905062D+04, &
    2.426813369486704502836312D+06, &
    1.214755574045093227939592D+08, &
    2.663432449630976949898078D+09, &
    2.940378956634553899906876D+10, &
    1.702665737765398868392998D+11, &
    4.926125793377430887588120D+11, &
    5.606251856223951465078242D+11 /)
  real ( kind = 8 ), dimension ( 8 ) :: q1 = (/ &
    6.748212550303777196073036D+01, &
    1.113332393857199323513008D+03, &
    7.738757056935398733233834D+03, &
    2.763987074403340708898585D+04, &
    5.499310206226157329794414D+04, &
    6.161122180066002127833352D+04, &
    3.635127591501940507276287D+04, &
    8.785536302431013170870835D+03 /)
  real ( kind = 8 ), dimension ( 8 ) :: q2 = (/ &
    1.830328399370592604055942D+02, &
    7.765049321445005871323047D+03, &
    1.331903827966074194402448D+05, &
    1.136705821321969608938755D+06, &
    5.267964117437946917577538D+06, &
    1.346701454311101692290052D+07, &
    1.782736530353274213975932D+07, &
    9.533095591844353613395747D+06 /)
  real ( kind = 8 ), dimension ( 8 ) :: q4 = (/ &
    2.690530175870899333379843D+03, &
    6.393885654300092398984238D+05, &
    4.135599930241388052042842D+07, &
    1.120872109616147941376570D+09, &
    1.488613728678813811542398D+10, &
    1.016803586272438228077304D+11, &
    3.417476345507377132798597D+11, &
    4.463158187419713286462081D+11 /)
  real ( kind = 8 ) r8_gamma_log
  real ( kind = 8 ) res
  real ( kind = 8 ), parameter :: sqrtpi = 0.9189385332046727417803297D+00
  real ( kind = 8 ) x
  real ( kind = 8 ), parameter :: xbig = 2.55D+305
  real ( kind = 8 ) xden
  real ( kind = 8 ), parameter :: xinf = 1.79D+308
  real ( kind = 8 ) xm1
  real ( kind = 8 ) xm2
  real ( kind = 8 ) xm4
  real ( kind = 8 ) xnum
  real ( kind = 8 ) y
  real ( kind = 8 ) ysq

  y = x

  if ( 0.0D+00 < y .and. y <= xbig ) then

    if ( y <= epsilon ( y ) ) then

      res = - log ( y )
!
!  EPS < X <= 1.5.
!
    else if ( y <= 1.5D+00 ) then

      if ( y < 0.6796875D+00 ) then
        corr = -log ( y )
        xm1 = y
      else
        corr = 0.0D+00
        xm1 = ( y - 0.5D+00 ) - 0.5D+00
      end if

      if ( y <= 0.5D+00 .or. 0.6796875D+00 <= y ) then

        xden = 1.0D+00
        xnum = 0.0D+00
        do i = 1, 8
          xnum = xnum * xm1 + p1(i)
          xden = xden * xm1 + q1(i)
        end do

        res = corr + ( xm1 * ( d1 + xm1 * ( xnum / xden ) ) )

      else

        xm2 = ( y - 0.5D+00 ) - 0.5D+00
        xden = 1.0D+00
        xnum = 0.0D+00
        do i = 1, 8
          xnum = xnum * xm2 + p2(i)
          xden = xden * xm2 + q2(i)
        end do

        res = corr + xm2 * ( d2 + xm2 * ( xnum / xden ) )

      end if
!
!  1.5 < X <= 4.0.
!
    else if ( y <= 4.0D+00 ) then

      xm2 = y - 2.0D+00
      xden = 1.0D+00
      xnum = 0.0D+00
      do i = 1, 8
        xnum = xnum * xm2 + p2(i)
        xden = xden * xm2 + q2(i)
      end do

      res = xm2 * ( d2 + xm2 * ( xnum / xden ) )
!
!  4.0 < X <= 12.0.
!
    else if ( y <= 12.0D+00 ) then

      xm4 = y - 4.0D+00
      xden = -1.0D+00
      xnum = 0.0D+00
      do i = 1, 8
        xnum = xnum * xm4 + p4(i)
        xden = xden * xm4 + q4(i)
      end do

      res = d4 + xm4 * ( xnum / xden )
!
!  Evaluate for 12 <= argument.
!
    else

      res = 0.0D+00

      if ( y <= frtbig ) then

        res = c(7)
        ysq = y * y

        do i = 1, 6
          res = res / ysq + c(i)
        end do

      end if

      res = res / y
      corr = log ( y )
      res = res + sqrtpi - 0.5D+00 * corr
      res = res + y * ( corr - 1.0D+00 )

    end if
!
!  Return for bad arguments.
!
  else

    res = xinf

  end if
!
!  Final adjustments and return.
!
  r8_gamma_log = res

  return
end
function r8_haversine ( a )

!******************************************************************************/
!
!! R8_HAVERSINE computes the haversine of an angle.
!
!  Discussion:
!
!    haversine(A) = ( 1 - cos ( A ) ) / 2
!
!    The haversine is useful in spherical trigonometry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A, the angle.
!
!  Output:
!
!    double R8_HAVERSINE, the haversine of the angle.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) r8_haversine
  real ( kind = 8 ) value

  value = ( 1.0D+00 - cos ( a ) ) / 2.0

  r8_haversine = value

  return
end
function r8_heaviside ( x )

!*****************************************************************************80
!
!! R8_HEAVISIDE evaluates the Heaviside function.
!
!  Discussion:
!
!    The Heaviside function is 0 for x < 0, 1 for x > 0, and 1/2 for x = 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the argument.
!
!  Output:
!
!    real ( kind = 8 ) R8_HEAVISIDE, the value.
!
  implicit none

  real ( kind = 8 ) r8_heaviside
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    value = 0.0D+00
  else if ( x == 0.0D+00 ) then
    value = 0.5D+00
  else
    value = 1.0D+00
  end if

  r8_heaviside = value
  
  return
end
function r8_huge ( )

!*****************************************************************************80
!
!! R8_HUGE returns a very large R8.
!
!  Discussion:
!
!    The value returned by this function is intended to be the largest
!    representable real value.
!
!    FORTRAN90 provides a built-in routine HUGE ( X ) that
!    can return the maximum representable number of the same datatype
!    as X, if that is what is really desired.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Output:
!
!    real ( kind = 8 ) R8_HUGE, a "huge" value.
!
  implicit none

  real ( kind = 8 ) r8_huge
  real ( kind = 8 ), parameter :: t = 1.0D+00

  r8_huge = huge ( t )

  return
end
function r8_hypot ( x, y )

!*****************************************************************************80
!
!! R8_HYPOT returns the value of sqrt ( X^2 + Y^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, Y, the arguments.
!
!  Output:
!
!    real ( kind = 8 ) R8_HYPOT, the value of sqrt ( X^2 + Y^2 ).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8_hypot
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( abs ( x ) < abs ( y ) ) then
    a = abs ( y )
    b = abs ( x )
  else
    a = abs ( x )
    b = abs ( y )
  end if
!
!  A contains the larger value.
!
  if ( a == 0.0D+00 ) then
    c = 0.0D+00
  else
    c = a * sqrt ( 1.0D+00 + ( b / a ) ** 2 )
  end if

  r8_hypot = c

  return
end
function r8_is_in_01 ( a )

!*****************************************************************************80
!
!! R8_IN_01 is TRUE if an R8 is in the range [0,1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A, the value.
!
!  Output:
!
!    logical ( kind = 4 ) R8_IS_IN_01, is TRUE if 0 <= A <= 1.
!
  implicit none

  real ( kind = 8 ) a
  logical ( kind = 4 ) r8_is_in_01
  logical ( kind = 4 ) value

  if ( a < 0.0D+00 .or. 1.0D+00 < a ) then
    value = .false.
  else
    value = .true.
  end if

  r8_is_in_01 = value

  return
end
function r8_is_inf ( r )

!*****************************************************************************80
!
!! R8_IS_INF determines if an R8 represents an infinite value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 May 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, the number to be checked.
!
!  Output:
!
!    logical ( kind = 4 ) R8_IS_INF, is TRUE if R is an infinite value.
!
  implicit none

  real ( kind = 8 ) r
  logical ( kind = 4 ) r8_is_inf

  if ( r < 0.0D+00 ) then
    r8_is_inf = ( r < - huge ( r ) )
  else
    r8_is_inf = ( huge ( r ) < r )
  end if

  return
end
function r8_is_insignificant ( r, s )

!*****************************************************************************80
!
!! R8_IS_INSIGNIFICANT determines if an R8 is insignificant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, the number to be compared against.
!
!    real ( kind = 8 ) S, the number to be compared.
!
!  Output:
!
!    logical ( kind = 4 ) R8_IS_INSIGNIFICANT, is TRUE if S is 
!    insignificant compared to R.
!
  implicit none

  real ( kind = 8 ) r
  logical ( kind = 4 ) r8_is_insignificant
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  logical ( kind = 4 ) value

  value = .true. 

  t = r + s
  tol = epsilon ( r ) * abs ( r )

  if ( tol < abs ( r - t ) ) then 
    value = .false.
  end if
  
  r8_is_insignificant = value

  return
end
function r8_is_integer ( r )

!*****************************************************************************80
!
!! R8_IS_INTEGER determines if an R8 represents an integer value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, the number to be checked.
!
!  Output:
!
!    logical ( kind = 4 ) R8_IS_INTEGER, is TRUE if R is an integer.
!
  implicit none

  real ( kind = 8 ) r
  logical ( kind = 4 ) r8_is_integer

  r8_is_integer = ( r == aint ( r ) )

  return
end
function r8_is_nan ( r )

!*****************************************************************************80
!
!! R8_IS_NAN determines if an R8 represents a NaN value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 May 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, the number to be checked.
!
!  Output:
!
!    logical ( kind = 4 ) R8_IS_NAN, is TRUE if R is a NaN
!
  implicit none

  real ( kind = 8 ) r
  logical ( kind = 4 ) r8_is_nan

  r8_is_nan = ( r /= r )

  return
end
function r8_log_2 ( x )

!*****************************************************************************80
!
!! R8_LOG_2 returns the logarithm base 2 of an R8.
!
!  Discussion:
!
!    value = Log ( |X| ) / Log ( 2.0 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2002
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose base 2 logarithm is desired.
!    X should not be 0.
!
!  Output:
!
!    real ( kind = 8 ) R8_LOG_2, the logarithm base 2 of the absolute
!    value of X.  It should be true that |X| = 2^R8_LOG_2.
!
  implicit none

  real ( kind = 8 ) r8_log_2
  real ( kind = 8 ) x

  if ( x == 0.0D+00 ) then
    r8_log_2 = - huge ( x )
  else
    r8_log_2 = log ( abs ( x ) ) / log ( 2.0D+00 )
  end if

  return
end
function r8_log_10 ( x )

!*****************************************************************************80
!
!! R8_LOG_10 returns the logarithm base 10 of an R8.
!
!  Discussion:
!
!    value = Log10 ( |X| )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2002
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose base 2 logarithm is desired.
!    X should not be 0.
!
!  Output:
!
!    real ( kind = 8 ) R8_LOG_10, the logarithm base 10 of the absolute
!    value of X.  It should be true that |X| = 10^R8_LOG_10.
!
  implicit none

  real ( kind = 8 ) r8_log_10
  real ( kind = 8 ) x

  if ( x == 0.0D+00 ) then
    r8_log_10 = - huge ( x )
  else
    r8_log_10 = log10 ( abs ( x ) )
  end if

  return
end
function r8_log_b ( x, b )

!*****************************************************************************80
!
!! R8_LOG_B returns the logarithm base B of an R8.
!
!  Discussion:
!
!    value = log ( |X| ) / log ( |B| )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2002
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose base B logarithm is desired.
!    X should not be 0.
!
!    real ( kind = 8 ) B, the base, which should not be 0, 1 or -1.
!
!  Output:
!
!    real ( kind = 8 ) R8_LOG_B, the logarithm base B of the absolute
!    value of X.  It should be true that |X| = |B|^R8_LOG_B.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) r8_log_b
  real ( kind = 8 ) x

  if ( b == 0.0D+00 .or. b == 1.0D+00 .or. b == - 1.0D+00 ) then
    r8_log_b = - huge ( x )
  else if ( abs ( x ) == 0.0D+00 ) then
    r8_log_b = - huge ( x )
  else
    r8_log_b = log ( abs ( x ) ) / log ( abs ( b ) )
  end if

  return
end
subroutine r8_mant ( x, s, r, l )

!*****************************************************************************80
!
!! R8_MANT computes the "mantissa" or "fraction part" of an R8.
!
!  Discussion:
!
!    X = S * R * 2^L
!
!    S is +1 or -1,
!    R is an real value between 1.0 and 2.0,
!    L is an integer.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number to be decomposed.
!
!  Output:
!
!    integer ( kind = 4 ) S, the "sign" of the number.
!    S will be -1 if X is less than 0, and +1 if X is greater
!    than or equal to zero.
!
!    real ( kind = 8 ) R, the mantissa of X.  R will be greater
!    than or equal to 1, and strictly less than 2.  The one
!    exception occurs if X is zero, in which case R will also
!    be zero.
!
!    integer ( kind = 4 ) L, the integer part of the logarithm
!    (base 2) of X.
!
  implicit none

  integer ( kind = 4 ) l
  real ( kind = 8 ) r
  integer ( kind = 4 ) s
  real ( kind = 8 ) x
!
!  Determine the sign.
!
  if ( x < 0.0D+00 ) then
    s = -1
  else
    s = + 1
  end if
!
!  Set R to the absolute value of X, and L to zero.
!  Then force R to lie between 1 and 2.
!
  if ( x < 0.0D+00 ) then
    r = - x
  else
    r = + x
  end if

  l = 0
!
!  Time to bail out if X is zero.
!
  if ( x == 0.0D+00 ) then
    return
  end if

  do while ( 2.0D+00 <= r )
    r = r / 2.0D+00
    l = l + 1
  end do

  do while ( r < 1.0D+00 )
    r = r * 2.0D+00
    l = l - 1
  end do

  return
end
function r8_max ( x, y )

!*****************************************************************************80
!
!! R8_MAX returns the maximum of two R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, Y, the numbers to compare.
!
!  Output:
!
!    real ( kind = 8 ) R8_MAX, the maximum of X and Y.
!
  implicit none

  real ( kind = 8 ) r8_max
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x < y ) then
    r8_max = y
  else
    r8_max = x
  end if

  return
end
function r8_min ( x, y )

!*****************************************************************************80
!
!! R8_MIN returns the minimum of two R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, Y, the numbers to compare.
!
!  Output:
!
!    real ( kind = 8 ) R8_MIN, the minimum of X and Y.
!
  implicit none

  real ( kind = 8 ) r8_min
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x < y ) then
    r8_min = x
  else
    r8_min = y
  end if

  return
end
function r8_mod ( x, y )

!*****************************************************************************80
!
!! R8_MOD returns the remainder of R8 division.
!
!  Discussion:
!
!    If
!      REM = R8_MOD ( X, Y )
!      RMULT = ( X - REM ) / Y
!    then
!      X = Y * RMULT + REM
!    where REM has the same sign as X, and abs ( REM ) < Y.
!
!  Example:
!
!        X         Y     R8_MOD  R8_MOD Factorization
!
!      107        50       7      107 =  2 *  50 + 7
!      107       -50       7      107 = -2 * -50 + 7
!     -107        50      -7     -107 = -2 *  50 - 7
!     -107       -50      -7     -107 =  2 * -50 - 7
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number to be divided.
!
!    real ( kind = 8 ) Y, the number that divides X.
!
!  Output:
!
!    real ( kind = 8 ) R8_MOD, the remainder when X is divided by Y.
!
  implicit none

  real ( kind = 8 ) r8_mod
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( y == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_MOD - Fatal error!'
    write ( *, '(a,g14.6)' ) '  R8_MOD ( X, Y ) called with Y = ', y
    stop 1
  end if

  r8_mod = x - real ( int ( x / y ), kind = 8 ) * y

  if ( x < 0.0D+00 .and. 0.0D+00 < r8_mod ) then
    r8_mod = r8_mod - abs ( y )
  else if ( 0.0D+00 < x .and. r8_mod < 0.0D+00 ) then
    r8_mod = r8_mod + abs ( y )
  end if

  return
end
function r8_modp ( x, y )

!*****************************************************************************80
!
!! R8_MODP returns the nonnegative remainder of R8 division.
!
!  Discussion:
!
!    If
!      REM = R8_MODP ( X, Y )
!      RMULT = ( X - REM ) / Y
!    then
!      X = Y * RMULT + REM
!    where REM is always nonnegative.
!
!    The MOD function computes a result with the same sign as the
!    quantity being divided.  Thus, suppose you had an angle A,
!    and you wanted to ensure that it was between 0 and 360.
!    Then mod(A,360.0) would do, if A was positive, but if A
!    was negative, your result would be between -360 and 0.
!
!    On the other hand, R8_MODP(A,360.0) is between 0 and 360, always.
!
!  Example:
!
!        X         Y     MOD R8_MODP  R8_MODP Factorization
!
!      107        50       7       7    107 =  2 *  50 + 7
!      107       -50       7       7    107 = -2 * -50 + 7
!     -107        50      -7      43   -107 = -3 *  50 + 43
!     -107       -50      -7      43   -107 =  3 * -50 + 43
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number to be divided.
!
!    real ( kind = 8 ) Y, the number that divides X.
!
!  Output:
!
!    real ( kind = 8 ) R8_MODP, the nonnegative remainder
!    when X is divided by Y.
!
  implicit none

  real ( kind = 8 ) r8_modp
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( y == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_MODP - Fatal error!'
    write ( *, '(a,g14.6)' ) '  R8_MODP ( X, Y ) called with Y = ', y
    stop 1
  end if

  r8_modp = mod ( x, y )

  if ( r8_modp < 0.0D+00 ) then
    r8_modp = r8_modp + abs ( y )
  end if

  return
end
function r8_mop ( i )

!*****************************************************************************80
!
!! R8_MOP returns the I-th power of -1 as an R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 November 2007
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) I, the power of -1.
!
!  Output:
!
!    real ( kind = 8 ) R8_MOP, the I-th power of -1.
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_mop
  real ( kind = 8 ) value

  if ( mod ( i, 2 ) == 0 ) then
    value = + 1.0D+00
  else
    value = - 1.0D+00
  end if

  r8_mop = value

  return
end
function r8_nan ( )

!*****************************************************************************80
!
!! R8_NAN returns the value of NaN as an R8.
!
!  Discussion:
!
!    Some compilers have a NaN function built in.
!
!    So far I haven't found a simple portable way of implementing 
!    this function!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Output:
!
!    real ( kind = 8 ) R8_NAN, a NaN value.
!
! use, intrinsic :: iso_fortran_env
! use, intrinsic :: ieee_arithmetic

  real ( kind = 8 ) r8_nan
  integer ( kind = 4 ) r8_nint
!
!  My "clever" compiler won't compile:
!
!    r8_nan = 0.0D+00 / 0.0D+00
!
!  Maybe something stupid like this will work.
!
   r8_nan = r8_nint ( 0.1D+00) / r8_nint ( 0.2D+00 )
!
!  Something smart like this is just bound not to work.
!
! value = ieee_value ( value, ieee_quiet_nan )
! r8_nan = value
 
  return
end
function r8_nint ( x )

!*****************************************************************************80
!
!! R8_NINT returns the nearest integer to an R8.
!
!  Example:
!
!        X        R8_NINT
!
!      1.3         1
!      1.4         1
!      1.5         1 or 2
!      1.6         2
!      0.0         0
!     -0.7        -1
!     -1.1        -1
!     -1.6        -2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the value.
!
!  Output:
!
!    integer ( kind = 4 ) R8_NINT, the nearest integer to X.
!
  implicit none

  integer ( kind = 4 ) r8_nint
  integer ( kind = 4 ) s
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    s = - 1
  else
    s = + 1
  end if

  r8_nint = s * int ( abs ( x ) + 0.5D+00 )

  return
end
function r8_normal_01 ( seed )

!*****************************************************************************80
!
!! R8_NORMAL_01 returns a unit pseudonormal R8.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) SEED, a seed for the random number generator.
!
!  Output:
!
!    real ( kind = 8 ) R8_NORMAL_01, a sample of the standard
!    normal PDF.
!
!    integer ( kind = 4 ) SEED, an updated seed.
!
  implicit none

  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_normal_01
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  r1 = r8_uniform_01 ( seed )
  r2 = r8_uniform_01 ( seed )
  x = sqrt ( - 2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * r8_pi * r2 )

  r8_normal_01 = x

  return
end
function r8_normal_ab ( a, b, seed )

!*****************************************************************************80
!
!! R8_NORMAL_AB returns a scaled pseudonormal R8.
!
!  Discussion:
!
!    The normal probability distribution function (PDF) is sampled,
!    with mean A and standard deviation B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A, the mean of the PDF.
!
!    real ( kind = 8 ) B, the standard deviation of the PDF.
!
!    integer ( kind = 4 ) SEED, a seed for the random number generator.
!
!  Output:
!
!    real ( kind = 8 ) R8_NORMAL_AB, a sample of the normal PDF.
!
!    integer ( kind = 4 ) SEED, an updated seed.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) r8_normal_ab
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  r1 = r8_uniform_01 ( seed )
  r2 = r8_uniform_01 ( seed )
  x = sqrt ( - 2.0D+00 * log ( r1 ) ) * cos ( 2.0D+00 * r8_pi * r2 )

  r8_normal_ab = a + b * x

  return
end
function r8_nth_root ( x, n )

!*****************************************************************************80
!
!! R8_NTH_ROOT returns the nth-root of an R8.
!
!  Discussion:
!
!    The nth root of X is x^(1/n)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose Nth root is desired.
!
!    integer ( kind = 4 ) N, the index of the root.
!
!  Output:
!
!    real ( kind = 8 ) R8_NTH_ROOT, the Nth root of X.
!
  implicit none

  real ( kind = 8 ) e
  integer ( kind = 4 ) n
! real ( kind = 8 ) r8_nan
  real ( kind = 8 ) r8_nth_root
  real ( kind = 8 ) value
  real ( kind = 8 ) x
!
!  Potential Error 1: 0^0
!  But we will use it as 1.
!
  if ( x == 0.0D+00 .and. n == 0 ) then

    value = 1.0;
!
!  Error 2: 0^(negative power)
!  Returned as Inf on my system.
!
  else if ( x == 0.0D+00 .and. n < 0 ) then

    value = x ** ( 1.0 / real ( n, kind = 8 ) )
!
!  Error 3: (negative)^(even strictly positive root)
!  Returned as NaN on my system.
!
  else if ( x < 0.0D+00 .and. mod ( n, 2 ) == 0 .and. 0 < n ) then

    value = x ** ( 1.0 / real ( n, kind = 8 ) )
!
!  X^0 = 1
!
  else if ( n == 0 ) then

    value = 1.0D+00
!
!  X^1 = X
!
  else if ( n == 1 ) then

    value = x
!
!  X^(-1) = 1/X
!
  else if ( n == -1 ) then

    value = 1.0D+00 / x

  else
  
    e = 1.0D+00 / abs ( n )

    if ( 0.0 < x ) then
      value = x ** e
    else if ( x == 0.0D+00 ) then
      value = 0.0D+00
    else
      value = - ( abs ( x ) ) ** e
    end if

    if ( n < 0 ) then
      value = 1.0D+00 / value
    end if

  end if

  r8_nth_root = value

  return
end
function r8_pi ( )

!*****************************************************************************80
!
!! R8_PI returns the value of pi as an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Output:
!
!    real ( kind = 8 ) R8_PI, the value of pi.
!
  implicit none

  real ( kind = 8 ) r8_pi

  r8_pi = 3.141592653589793D+00

  return
end
function r8_pi_sqrt ( )

!*****************************************************************************80
!
!! R8_PI_SQRT returns the square root of pi as an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Output:
!
!    real ( kind = 8 ) R8_PI_SQRT, the square root of pi.
!
  implicit none

  real ( kind = 8 ) r8_pi_sqrt

  r8_pi_sqrt = 1.7724538509055160273D+00

  return
end
function r8_power ( r, p )

!*****************************************************************************80
!
!! R8_POWER computes the P-th power of an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, the base.
!
!    integer ( kind = 4 ) P, the power, which may be negative.
!
!  Output:
!
!    real ( kind = 8 ) R8_POWER, the value of the P-th power of R.
!
  implicit none

  integer ( kind = 4 ) p
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_power
  real ( kind = 8 ) value
!
!  Special case.  R^0 = 1.
!
  if ( p == 0 ) then

    value = 1.0D+00
!
!  Special case.  Positive powers of 0 are 0.
!  For negative powers of 0, we go ahead and compute R^P,
!  relying on the software to complain.
!
  else if ( r == 0.0D+00 ) then

    if ( 0 < p ) then
      value = 0.0D+00
    else
      value = r ** p
    end if

  else if ( 1 <= p ) then
    value = r ** p
  else
    value = 1.0D+00 / r ** ( - p )
  end if

  r8_power = value

  return
end
subroutine r8_power_fast ( r, p, rp, mults )

!*****************************************************************************80
!
!! R8_POWER_FAST computes an integer power of an R8.
!
!  Discussion:
!
!    Obviously, R^P can be computed using P-1 multiplications.
!
!    However, R^P can also be computed using at most 2*LOG2(P) multiplications.
!    To do the calculation this way, let N = LOG2(P).
!    Compute A, A^2, A^4, ..., A^N by N-1 successive squarings.
!    Start the value of R^P at A, and each time that there is a 1 in
!    the binary expansion of P, multiply by the current result of the squarings.
!
!    This algorithm is not optimal.  For small exponents, and for special
!    cases, the result can be computed even more quickly.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, the base.
!
!    integer ( kind = 4 ) P, the power, which may be negative.
!
!  Output:
!
!    real ( kind = 8 ) RP, the value of R^P.
!
!    integer ( kind = 4 ) MULTS, the number of multiplications
!    and divisions.
!
  implicit none

  integer ( kind = 4 ) mults
  integer ( kind = 4 ) p
  integer ( kind = 4 ) p_mag
  integer ( kind = 4 ) p_sign
  real ( kind = 8 ) r
  real ( kind = 8 ) r2
  real ( kind = 8 ) rp

  mults = 0
!
!  Special bases.
!
  if ( r == 1.0D+00 ) then
    rp = 1.0D+00
    return
  end if

  if ( r == -1.0D+00 ) then

    if ( mod ( p, 2 ) == 1 ) then
      rp = -1.0D+00
    else
      rp = 1.0D+00
    end if

    return

  end if

  if ( r == 0.0D+00 ) then

    if ( p <= 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_POWER_FAST - Fatal error!'
      write ( *, '(a)' ) '  Base R is zero, and exponent is negative.'
      write ( *, '(a,i8)' ) '  Exponent P = ', p
      stop 1
    end if

    rp = 0.0D+00
    return

  end if
!
!  Special powers.
!
  if ( p == -1 ) then
    rp = 1.0D+00 / r
    mults = mults + 1
    return
  else if ( p == 0 ) then
    rp = 1.0D+00
    return
  else if ( p == 1 ) then
    rp = r
    return
  end if
!
!  Some work to do.
!
  p_mag = abs ( p )
  p_sign = sign ( 1, p )

  rp = 1.0D+00
  r2 = r

  do while ( 0 < p_mag )

    if ( mod ( p_mag, 2 ) == 1 ) then
      rp = rp * r2
      mults = mults + 1
    end if

    p_mag = p_mag / 2
    r2 = r2 * r2
    mults = mults + 1

  end do

  if ( p_sign == -1 ) then
    rp = 1.0D+00 / rp
    mults = mults + 1
  end if

  return
end
subroutine r8_print ( r, title )

!*****************************************************************************80
!
!! R8_PRINT prints an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, the value.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  real ( kind = 8 ) r
  character ( len = * ) title

  write ( *, '(a,2x,g14.6)' ) trim ( title ), r

  return
end
function r8_pythag ( a, b )

!*****************************************************************************80
!
!! R8_PYTHAG computes sqrt ( A * A + B * B ) as an R8.
!
!  Discussion:
!
!    The computation avoids unnecessary overflow and underflow.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2017
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A, B, the values for which sqrt ( A * A + B * B )
!    is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8_PYTHAG, the value of sqrt ( A * A + B * B ).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a_abs
  real ( kind = 8 ) b
  real ( kind = 8 ) b_abs
  real ( kind = 8 ) r8_pythag

  a_abs = abs ( a )
  b_abs = abs ( b )

  if ( b_abs .lt. a_abs ) then
    r8_pythag = a_abs * sqrt ( 1.0D+00 + ( b_abs / a_abs ) * ( b_abs / a_abs ) )
  else if ( b_abs .eq. 0.0D+00 ) then
    r8_pythag = 0.0D+00
  else if ( a_abs .le. b_abs ) then
    r8_pythag = b_abs * sqrt ( 1.0D+00 + ( a_abs / b_abs ) * ( a_abs / b_abs ) )
  end if

  return
end
function r8_radians ( degrees )

!*****************************************************************************80
!
!! R8_RADIANS converts an angle from degree to radian measure.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) DEGREES, the angle measurement in degrees.
!
!  Output:
!
!    real ( kind = 8 ) R8_RADIANS, the angle measurement in radians.
!
  implicit none

  real ( kind = 8 ) degrees
  real ( kind = 8 ), parameter :: r8_pi = 3.1415926535897932384626434D+00
  real ( kind = 8 ) r8_radians

  r8_radians = degrees * r8_pi / 180.0D+00

  return
end
function r8_relu ( x )

!*****************************************************************************80
!
!! R8_RELU evaluates the ReLU function of an R8.
!
!  Discussion:
!
!    An R8 is a double precision real value.
!
!    The ReLU function is a max(x,0.0).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 January 2019
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the argument.
!
!  Output:
!
!    real ( kind = 8 ) R8_RELU, the function value.
!
  implicit none

  real ( kind = 8 ) r8_relu
  real ( kind = 8 ) x

  r8_relu = max ( x, 0.0D+00 );

  return
end
function r8_rise ( x, n )

!*****************************************************************************80
!
!! R8_RISE computes the rising factorial function [X]^N.
!
!  Discussion:
!
!    [X]^N = X * ( X + 1 ) * ( X + 2 ) * ... * ( X + N - 1 ).
!
!    Note that the number of ways of arranging N objects in M ordered
!    boxes is [M]^N.  (Here, the ordering of the objects in each box matters).
!    Thus, 2 objects in 2 boxes have the following 6 possible arrangements:
!
!      -|12, 1|2, 12|-, -|21, 2|1, 21|-.
!
!    Moreover, the number of non-decreasing maps from a set of
!    N to a set of M ordered elements is [M]^N / N!.  Thus the set of
!    nondecreasing maps from (1,2,3) to (a,b,c,d) is the 20 elements:
!
!      aaa, abb, acc, add, aab, abc, acd, aac, abd, aad
!      bbb, bcc, bdd, bbc, bcd, bbd, ccc, cdd, ccd, ddd.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the argument of the rising factorial function.
!
!    integer ( kind = 4 ) N, the order of the rising factorial function.
!    If N = 0, RISE = 1, if N = 1, RISE = X.  Note that if N is
!    negative, a "falling" factorial will be computed.
!
!  Output:
!
!    real ( kind = 8 ) R8_RISE, the value of the rising factorial
!    function.
!
  implicit none

  real ( kind = 8 ) arg
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_rise
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  value = 1.0D+00

  arg = x

  if ( 0 < n ) then

    do i = 1, n
      value = value * arg
      arg = arg + 1.0D+00
    end do

  else if ( n < 0 ) then

    do i = -1, n, -1
      value = value * arg
      arg = arg - 1.0D+00
    end do

  end if

  r8_rise = value

  return
end
subroutine r8_rise_values ( n_data, x, n, f )

!*****************************************************************************80
!
!! R8_RISE_VALUES returns some values of the rising factorial function.
!
!  Discussion:
!
!    Pochhammer(X,Y) = Gamma(X+Y) / Gamma(X)
!
!    For integer arguments, Pochhammer(M,N) = ( M + N - 1 )! / ( N - 1 )!
!
!    In Mathematica, the function can be evaluated by:
!
!      Pochhammer[X,Y]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 December 2014
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 before 
!    the first call.  
!
!  Output:
!
!    integer ( kind = 4 ) N_DATA.  On each call, the routine increments 
!    N_DATA by 1, and returns the corresponding data; when there is no 
!    more data, the output value of N_DATA will be 0 again.
!
!    real ( kind = 8 ) X, integer ( kind = 4 ) N, the arguments.
!
!    real ( kind = 8 ) F, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 15

  real ( kind = 8 ), save, dimension ( n_max ) :: f_vec = (/ &
       1680.000000000000D+00, &
       1962.597656250000D+00, &
       2279.062500000000D+00, &
       2631.972656250000D+00, &
       3024.000000000000D+00, &
       1.000000000000000D+00, &
       7.500000000000000D+00, &
       63.75000000000000D+00, &
       605.6250000000000D+00, &
       6359.062500000000D+00, &
       73129.21875000000D+00, &
       914115.2343750000D+00, &
       1.234055566406250D+07, &
       1.789380571289063D+08, &
       2.773539885498047D+09 /)
  real ( kind = 8 ) f
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ), save, dimension ( n_max ) :: n_vec = (/ &
       4, &
       4, &
       4, &
       4, &
       4, &
       0, &
       1, &
       2, &
       3, &
       4, &
       5, &
       6, &
       7, &
       8, &
       9 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
       5.00D+00, &
       5.25D+00, &
       5.50D+00, &
       5.75D+00, &
       6.00D+00, &
       7.50D+00, &
       7.50D+00, &
       7.50D+00, &
       7.50D+00, &
       7.50D+00, &
       7.50D+00, &
       7.50D+00, &
       7.50D+00, &
       7.50D+00, &
       7.50D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    x = 0.0D+00
    n = 0
    f = 0.0D+00
  else
    x = x_vec(n_data)
    n = n_vec(n_data)
    f = f_vec(n_data)
  end if

  return
end
function r8_round ( x )

!*****************************************************************************80
!
!! R8_ROUND sets an R8 to the nearest integral value.
!
!  Example:
!
!        X        R8_ROUND
!
!      1.3         1.0
!      1.4         1.0
!      1.5         1.0 or 2.0
!      1.6         2.0
!      0.0         0.0
!     -0.7        -1.0
!     -1.1        -1.0
!     -1.6        -2.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the value.
!
!  Output:
!
!    real ( kind = 8 ) R8_ROUND, the rounded value.
!
  implicit none

  real ( kind = 8 ) r8_round
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    value = - real ( int ( - x + 0.5D+00 ), kind = 8 )
  else
    value =   real ( int ( + x + 0.5D+00 ), kind = 8 )
  end if

  r8_round = value

  return
end
function r8_round_i4 ( x )

!*****************************************************************************80
!
!! R8_ROUND_I4 sets an R8 to the nearest integral value, returning an I4
!
!  Example:
!
!        X        R8_ROUND_I4
!
!      1.3         1
!      1.4         1
!      1.5         1 or 2
!      1.6         2
!      0.0         0
!     -0.7        -1
!     -1.1        -1
!     -1.6        -2
!
!  Discussion:
!
!    In FORTRAN90, we rely on the fact that, for positive X, int ( X )
!    is the "floor" function, returning the largest integer less than
!    or equal to X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the value.
!
!  Output:
!
!    integer ( kind = 4 ) R8_ROUND_I4, the rounded value.
!
  implicit none

  integer ( kind = 4 ) r8_round_i4
  integer ( kind = 4 ) value
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    value = - int ( - x + 0.5D+00 )
  else
    value =   int ( + x + 0.5D+00 )
  end if

  r8_round_i4 = value

  return
end
subroutine r8_round2 ( nplace, x, xround )

!*****************************************************************************80
!
!! R8_ROUND2 rounds an R8 in base 2.
!
!  Discussion:
!
!    Assume that the input quantity X has the form
!
!      X = S * J * 2^L
!
!    where S is plus or minus 1, L is an integer, and J is a binary
!    mantissa which is either exactly zero, or greater than or equal
!    to 0.5 and strictly less than 1.0.
!
!    Then on return, XROUND will satisfy
!
!      XROUND = S * K * 2^L
!
!    where S and L are unchanged, and K is a binary mantissa which
!    agrees with J in the first NPLACE binary digits and is zero
!    thereafter.
!
!    If NPLACE is 0, XROUND will always be zero.
!
!    If NPLACE is 1, the mantissa of XROUND will be 0 or 0.5.
!
!    If NPLACE is 2, the mantissa of XROUND will be 0, 0.25, 0.50,
!    or 0.75.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) NPLACE, the number of binary digits to
!    preserve.  NPLACE should be 0 or positive.
!
!    real ( kind = 8 ) X, the number to be decomposed.
!
!  Output:
!
!    real ( kind = 8 ) XROUND, the rounded value of X.
!
  implicit none

  integer ( kind = 4 ) iplace
  integer ( kind = 4 ) l
  integer ( kind = 4 ) nplace
  integer ( kind = 4 ) s
  real ( kind = 8 ) x
  real ( kind = 8 ) xmant
  real ( kind = 8 ) xround
  real ( kind = 8 ) xtemp

  xround = 0.0D+00
!
!  1: Handle the special case of 0.
!
  if ( x == 0.0D+00 ) then
    return
  end if

  if ( nplace <= 0 ) then
    return
  end if
!
!  2: Determine the sign S.
!
  if ( 0.0D+00 < x ) then
    s = 1
    xtemp = x
  else
    s = -1
    xtemp = -x
  end if
!
!  3: Force XTEMP to lie between 1 and 2, and compute the
!  logarithm L.
!
  l = 0

  do while ( 2.0D+00 <= xtemp )
    xtemp = xtemp / 2.0D+00
    l = l + 1
  end do

  do while ( xtemp < 1.0D+00 )
    xtemp = xtemp * 2.0D+00
    l = l - 1
  end do
!
!  4: Strip out the digits of the mantissa as XMANT, and decrease L.
!
  xmant = 0.0D+00
  iplace = 0

  do

    xmant = 2.0D+00 * xmant

    if ( 1.0D+00 <= xtemp ) then
      xmant = xmant + 1.0D+00
      xtemp = xtemp - 1.0D+00
    end if

    iplace = iplace + 1

    if ( xtemp == 0.0D+00 .or. nplace <= iplace ) then
      xround = s * xmant * 2.0D+00**l
      exit
    end if

    l = l - 1
    xtemp = xtemp * 2.0D+00

  end do

  return
end
subroutine r8_roundb ( base, nplace, x, xround )

!*****************************************************************************80
!
!! R8_ROUNDB rounds an R8 in a given base.
!
!  Discussion:
!
!    The code does not seem to do a good job of rounding when
!    the base is negative.
!
!    Assume that the input quantity X has the form
!
!      X = S * J * BASE^L
!
!    where S is plus or minus 1, L is an integer, and J is a
!    mantissa base BASE which is either exactly zero, or greater
!    than or equal to (1/BASE) and less than 1.0.
!
!    Then on return, XROUND will satisfy
!
!      XROUND = S * K * BASE^L
!
!    where S and L are unchanged, and K is a mantissa base BASE
!    which agrees with J in the first NPLACE digits and is zero
!    thereafter.
!
!    Note that because of rounding, for most bases, most numbers
!    with a fractional quantities cannot be stored exactly in the
!    computer, and hence will have trailing "bogus" digits.
!
!    If NPLACE is 0, XROUND will always be zero.
!
!    If NPLACE is 1, the mantissa of XROUND will be 0,
!    1/BASE, 2/BASE, ..., (BASE-1)/BASE.
!
!    If NPLACE is 2, the mantissa of XROUND will be 0,
!    BASE/BASE^2, (BASE+1)/BASE^2, ...,
!    BASE^2-2/BASE^2, BASE^2-1/BASE^2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) BASE, the base of the arithmetic.
!    BASE must not be zero.  Theoretically, BASE may be negative.
!
!    integer ( kind = 4 ) NPLACE, the number of digits base BASE to
!    preserve.  NPLACE should be 0 or positive.
!
!    real ( kind = 8 ) X, the number to be decomposed.
!
!  Output:
!
!    real ( kind = 8 ) XROUND, the rounded value of X.
!
  implicit none

  integer ( kind = 4 ) base
  integer ( kind = 4 ) iplace
  integer ( kind = 4 ) is
  integer ( kind = 4 ) js
  integer ( kind = 4 ) l
  integer ( kind = 4 ) nplace
  real ( kind = 8 ) x
  real ( kind = 8 ) xmant
  real ( kind = 8 ) xround
  real ( kind = 8 ) xtemp

  xround = 0.0D+00
!
!  0: Error checks.
!
  if ( base == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_ROUNDB - Fatal error!'
    write ( *, '(a)' ) '  The base BASE cannot be zero.'
    stop 1
  end if
!
!  1: Handle the special case of 0.
!
  if ( x == 0.0D+00 ) then
    return
  end if

  if ( nplace <= 0 ) then
    return
  end if
!
!  2: Determine the sign IS.
!
  if ( 0.0D+00 < x ) then
    is = 1
    xtemp = x
  else
    is = -1
    xtemp = -x
  end if
!
!  3: Force XTEMP to lie between 1 and ABS(BASE), and compute the
!  logarithm L.
!
  l = 0

  do while ( abs ( base ) <= abs ( xtemp ) )

    xtemp = xtemp / real ( base, kind = 8 )

    if ( xtemp < 0.0D+00 ) then
      is = -is
      xtemp = -xtemp
    end if

    l = l + 1

  end do

  do while ( abs ( xtemp ) < 1.0D+00 )

    xtemp = xtemp * base

    if ( xtemp < 0.0D+00 ) then
      is = -is
      xtemp = -xtemp
    end if

    l = l - 1

  end do
!
!  4: Now strip out the digits of the mantissa as XMANT, and
!  decrease L.
!
  xmant = 0.0D+00
  iplace = 0
  js = is

  do

    xmant = base * xmant

    if ( xmant < 0.0D+00 ) then
      js = -js
      xmant = -xmant
    end if

    if ( 1.0D+00 <= xtemp ) then
      xmant = xmant + int ( xtemp )
      xtemp = xtemp - int ( xtemp )
    end if

    iplace = iplace + 1

    if ( xtemp == 0.0D+00 .or. nplace <= iplace ) then
      xround = js * xmant * ( real ( base, kind = 8 ) ) ** l
      exit
    end if

    l = l - 1
    xtemp = xtemp * base

    if ( xtemp < 0.0D+00 ) then
      is = -is
      xtemp = -xtemp
    end if

  end do

  return
end
subroutine r8_roundx ( nplace, x, xround )

!*****************************************************************************80
!
!! R8_ROUNDX rounds an R8 in base 10.
!
!  Discussion:
!
!    Assume that the input quantity X has the form
!
!      X = S * J * 10^L
!
!    where S is plus or minus 1, L is an integer, and J is a decimal
!    mantissa which is either exactly zero, or greater than or equal
!    to 0.1 and less than 1.0.
!
!    Then on return, XROUND will satisfy
!
!      XROUND = S * K * 10^L
!
!    where S and L are unchanged, and K is a decimal mantissa which
!    agrees with J in the first NPLACE decimal digits and is zero
!    thereafter.
!
!    Note that because of rounding, most decimal fraction quantities
!    cannot be stored exactly in the computer, and hence will have
!    trailing "bogus" digits.
!
!    If NPLACE is 0, XROUND will always be zero.
!
!    If NPLACE is 1, the mantissa of XROUND will be 0, 0.1,
!    0.2, ..., or 0.9.
!
!    If NPLACE is 2, the mantissa of XROUND will be 0, 0.01, 0.02,
!    0.03, ..., 0.98, 0.99.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) NPLACE, the number of decimal digits to
!    preserve.  NPLACE should be 0 or positive.
!
!    real ( kind = 8 ) X, the number to be decomposed.
!
!  Output:
!
!    real ( kind = 8 ) XROUND, the rounded value of X.
!
  implicit none

  integer ( kind = 4 ) iplace
  integer ( kind = 4 ) is
  integer ( kind = 4 ) l
  integer ( kind = 4 ) nplace
  real ( kind = 8 ) x
  real ( kind = 8 ) xmant
  real ( kind = 8 ) xround
  real ( kind = 8 ) xtemp

  xround = 0.0D+00
!
!  1: Handle the special case of 0.
!
  if ( x == 0.0D+00 ) then
    return
  end if

  if ( nplace <= 0 ) then
    return
  end if
!
!  2: Determine the sign IS.
!
  if ( 0.0D+00 < x ) then
    is = 1
    xtemp = x
  else
    is = -1
    xtemp = -x
  end if
!
!  3: Force XTEMP to lie between 1 and 10, and compute the
!  logarithm L.
!
  l = 0

  do while ( 10.0D+00 <= x )
    xtemp = xtemp / 10.0D+00
    l = l + 1
  end do

  do while ( xtemp < 1.0D+00 )
    xtemp = xtemp * 10.0D+00
    l = l - 1
  end do
!
!  4: Now strip out the digits of the mantissa as XMANT, and
!  decrease L.
!
  xmant = 0.0D+00
  iplace = 0

  do

    xmant = 10.0D+00 * xmant

    if ( 1.0D+00 <= xtemp ) then
      xmant = xmant + int ( xtemp )
      xtemp = xtemp - int ( xtemp )
    end if

    iplace = iplace + 1

    if ( xtemp == 0.0D+00 .or. nplace <= iplace ) then
      xround = is * xmant * ( 10.0D+00**l )
      exit
    end if

    l = l - 1
    xtemp = xtemp * 10.0D+00

  end do

  return
end
function r8_secd ( degrees )

!*****************************************************************************80
!
!! R8_SECD returns the secant of an angle given in degrees.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) DEGREES, the angle in degrees.
!
!  Output:
!
!    real ( kind = 8 ) R8_SECD, the secant of the angle.
!
  implicit none

  real ( kind = 8 ) degrees
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_secd
  real ( kind = 8 ) radians

  radians = r8_pi * ( degrees / 180.0D+00 )
  r8_secd = 1.0D+00 / cos ( radians )

  return
end
function r8_sech ( x )

!*****************************************************************************80
!
!! R8_SECH evaluates the hyperbolic secant, while avoiding COSH overflow.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the argument of the function.
!
!  Output:
!
!    real ( kind = 8 ) R8_SECH, the value of the function.
!
  implicit none

  real ( kind = 8 ), parameter :: log_huge = 80.0D+00
  real ( kind = 8 ) r8_sech
  real ( kind = 8 ) x

  if ( log_huge < abs ( x ) ) then
    r8_sech = 0.0D+00
  else
    r8_sech = 1.0D+00 / cosh ( x )
  end if

  return
end
function r8_sigmoid ( l, b, m, x )

!*****************************************************************************80
!
!! r8_sigmoid evaluates the sigmoid or logistic function.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    The sigmoid function is useful for classification problems in
!    machine learning.  Its value is always between 0 and 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2019
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) l, the maximum value of the function.  This is often 1.
!
!    real ( kind = 8 ) b, the cutoff value, where the function equals l/2.
!    This is often 0.
!
!    real ( kind = 8 ) m, the slope, which determines the steepness of the curve
!    and the width of the uncertainty interval.  This is often 1.
!
!    real ( kind = 8 ) x, the argument.
!
!  Output:
!
!    real ( kind = 8 ) r8_sigmoid, the value.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) l
  real ( kind = 8 ) m
  real ( kind = 8 ) r8_sigmoid
  real ( kind = 8 ) x

  r8_sigmoid = l / ( 1.0D+00 + exp ( - m * ( x - b ) ) )

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
!    Note that the standard FORTRAN90 "sign" function is more complicated.
!    In particular,
!
!      Z = sign ( X, Y )
!
!    means that
!
!      Z =   |X| if 0 <= Y;
!          - |X| if Y < 0;
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
!  Input:
!
!    real ( kind = 8 ) X, the number whose sign is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8_SIGN, the sign of X:
!
  implicit none

  real ( kind = 8 ) r8_sign
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    value = -1.0D+00
  else
    value = +1.0D+00
  end if

  r8_sign = value

  return
end
function r8_sign3 ( x )

!*****************************************************************************80
!
!! R8_SIGN3 returns the three-way sign of an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose sign is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8_SIGN3, the sign of X:
!
  implicit none

  real ( kind = 8 ) r8_sign3
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    value = -1.0D+00
  else if ( x == 0.0D+00 ) then
    value = 0.0D+00
  else
    value = +1.0D+00
  end if

  r8_sign3 = value

  return
end
function r8_sign_char ( x )

!*****************************************************************************80
!
!! R8_SIGN_CHAR returns a character indicating the sign of an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the number whose sign is desired.
!
!  Output:
!
!    character R8_SIGN_CHAR, the sign of X, '-', '0' or '+'.
!
  implicit none

  character r8_sign_char
  character value
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    value = '-'
  else if ( x == 0.0D+00 ) then
    value = '0'
  else
    value = '+'
  end if

  r8_sign_char = value

  return
end
function r8_sign_match ( r1, r2 )

!*****************************************************************************80
!
!! R8_SIGN_MATCH is TRUE if two R8's are of the same sign.
!
!  Discussion:
!
!    This test could be coded numerically as
!
!      if ( 0 <= r1 * r2 ) then ...
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R1, R2, the values to check.
!
!  Output:
!
!    logical ( kind = 4 ) R8_SIGN_MATCH, is TRUE if 
!    ( R1 <= 0 and R2 <= 0 ) or ( 0 <= R1 and 0 <= R2 ).
!
  implicit none

  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  logical ( kind = 4 ) r8_sign_match

  r8_sign_match = ( r1 <= 0.0D+00 .and. r2 <= 0.0D+00 ) .or. &
                  ( 0.0D+00 <= r1 .and. 0.0D+00 <= r2 )

  return
end
function r8_sign_match_strict ( r1, r2 )

!*****************************************************************************80
!
!! R8_SIGN_MATCH_STRICT is TRUE if two R8's are of the same strict sign.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R1, R2, the values to check.
!
!  Output:
!
!    logical ( kind = 4 ) R8_SIGN_MATCH_STRICT, is TRUE if the 
!    signs match.
!
  implicit none

  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  logical ( kind = 4 ) r8_sign_match_strict

  r8_sign_match_strict = &
    (           r1 <  0.0D+00 .and. r2 <  0.0D+00 ) .or. &
    (           r1 == 0.0D+00 .and. r2 == 0.0D+00 ) .or. &
    ( 0.0D+00 < r1            .and.       0.0D+00 < r2 )

  return
end
function r8_sign_opposite ( r1, r2 )

!*****************************************************************************80
!
!! R8_SIGN_OPPOSITE is TRUE if two R8's are not of the same sign.
!
!  Discussion:
!
!    This test could be coded numerically as
!
!      if ( r1 * r2 <= 0.0 ) then ...
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R1, R2, the values to check.
!
!  Output:
!
!    logical ( kind = 4 ) R8_SIGN_OPPOSITE, is TRUE if 
!    ( R1 <= 0 and 0 <= R2 ) or ( R2 <= 0 and 0 <= R1 ).
!
  implicit none

  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  logical ( kind = 4 ) r8_sign_opposite

  r8_sign_opposite = ( r1 <= 0.0D+00 .and. 0.0D+00 <= r2 ) .or. &
                     ( r2 <= 0.0D+00 .and. 0.0D+00 <= r1 )

  return
end
function r8_sign_opposite_strict ( r1, r2 )

!*****************************************************************************80
!
!! R8_SIGN_OPPOSITE_STRICT is TRUE if two R8's are strictly of opposite sign.
!
!  Discussion:
!
!    This test could be coded numerically as
!
!      if ( r1 * r2 < 0.0 ) then ...
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R1, R2, the values to check.
!
!  Output:
!
!    logical ( kind = 4 ) R8_SIGN_OPPOSITE_STRICT, is TRUE if 
!    ( R1 < 0 and 0 < R2 ) or ( R2 < 0 and 0 < R1 ).
!
  implicit none

  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  logical ( kind = 4 ) r8_sign_opposite_strict

  r8_sign_opposite_strict = ( r1 < 0.0D+00 .and. 0.0D+00 < r2 ) .or. &
                            ( r2 < 0.0D+00 .and. 0.0D+00 < r1 )

  return
end
subroutine r8_sincos_sum ( a, b, d, e, f )

!*****************************************************************************80
!
!! R8_SINCOS_SUM simplifies a*sin(cx)+b*cos(cx).
!
!  Discussion:
!
!    The expression
!      a * sin ( c * x ) + b * cos ( c * x )
!    can be rewritten as
!      d * sin ( c * x + e )
!    or
!      d * cos ( c * x + f ) 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A, B, the coefficients in the linear combination.
!
!  Output:
!
!    real ( kind = 8 ) D, E, F, the new coefficient, and the shift for
!    sine or for cosine.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) d
  real ( kind = 8 ) e
  real ( kind = 8 ) f
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00

  d = sqrt ( a * a + b * b )
  e = atan2 ( b, a )
  f = atan2 ( b, a ) - r8_pi / 2.0D+00
  if ( f < - r8_pi ) then
    f = f + 2.0D+00 * r8_pi
  end if

  return
end
function r8_sind ( degrees )

!*****************************************************************************80
!
!! R8_SIND returns the sine of an angle given in degrees.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) DEGREES, the angle in degrees.
!
!  Output:
!
!    real ( kind = 8 ) R8_SIND, the sine of the angle.
!
  implicit none

  real ( kind = 8 ) degrees
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_sind
  real ( kind = 8 ) radians

  radians = r8_pi * ( degrees / 180.0D+00 )
  r8_sind  = sin ( radians )

  return
end
function r8_softplus ( x )

!*****************************************************************************80
!
!! R8_SOFTPLUS evaluates the softplus function of an R8.
!
!  Discussion:
!
!    An R8 is a double precision real value.
!
!    The softplus function is a smoothed (differentiable) version of max(x,0.0).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the argument.
!
!  Output:
!
!    real ( kind = 8 ) VALUE, the function value.
!
  implicit none

  real ( kind = 8 ) r8_softplus
  real ( kind = 8 ) value
  real ( kind = 8 ) x

  if ( x <= -36.841D+00 ) then
    value = 0.0D+00
  else if ( +36.841D+00 <= x ) then
    value = x
  else
    value = log ( 1.0D+00 + exp ( x ) )
  end if

  r8_softplus = value

  return
end
function r8_sqrt_i4 ( i )

!*****************************************************************************80
!
!! R8_SQRT_I4 returns the square root of an I4 as an R8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 June 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) I, the number whose square root is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8_SQRT_I4, the value of sqrt(I).
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_sqrt_i4

  r8_sqrt_i4 = sqrt ( real ( i, kind = 8 ) )

  return
end
subroutine r8_swap ( x, y )

!*****************************************************************************80
!
!! R8_SWAP swaps two R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, Y, the values to be interchanged.
!
!  Output:
!
!    real ( kind = 8 ) X, Y, the interchanged values.
!
  implicit none

  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  z = x
  x = y
  y = z

  return
end
subroutine r8_swap3 ( x, y, z )

!*****************************************************************************80
!
!! R8_SWAP3 swaps three R8's.
!
!  Example:
!
!    Input:
!
!      X = 1, Y = 2, Z = 3
!
!    Output:
!
!      X = 2, Y = 3, Z = 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, Y, Z, three values to be swapped.
!
!  Output:
!
!    real ( kind = 8 ) X, Y, Z, the swapped values.
!
  implicit none

  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  w = x
  x = y
  y = z
  z = w

  return
end
function r8_tand ( degrees )

!*****************************************************************************80
!
!! R8_TAND returns the tangent of an angle given in degrees.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) DEGREES, the angle in degrees.
!
!  Output:
!
!    real ( kind = 8 ) R8_TAND, the tangent of the angle.
!
  implicit none

  real ( kind = 8 ) degrees
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_tand
  real ( kind = 8 ) radians

  radians = r8_pi * ( degrees / 180.0D+00 )
  r8_tand  = tan ( radians )

  return
end
function r8_tiny ( )

!*****************************************************************************80
!
!! R8_TINY returns a very small but positive R8.
!
!  Discussion:
!
!    FORTRAN90 provides a built-in routine TINY ( X ) that
!    is more suitable for this purpose, returning the smallest positive
!    but normalized real number.
!
!    This routine does NOT try to provide an accurate value for TINY.
!    Instead, it simply returns a "reasonable" value, that is, a rather
!    small, but representable, real number.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Output:
!
!    real ( kind = 8 ) R8_TINY, a "tiny" value.
!
  implicit none

  real ( kind = 8 ) r8_tiny
  real ( kind = 8 ), parameter :: t = 1.0D+00

  r8_tiny = tiny ( t )

  return
end
subroutine r8_to_r8_discrete ( r, rmin, rmax, nr, rd )

!*****************************************************************************80
!
!! R8_TO_R8_DISCRETE maps R to RD in [RMIN, RMAX] with NR possible values.
!
!  Formula:
!
!    if ( R < RMIN ) then
!      RD = RMIN
!    else if ( RMAX < R ) then
!      RD = RMAX
!    else
!      T = nint ( ( NR - 1 ) * ( R - RMIN ) / ( RMAX - RMIN ) )
!      RD = RMIN + T * ( RMAX - RMIN ) / real ( NR - 1 )
!
!    In the special case where NR = 1, when
!
!      XD = 0.5 * ( RMAX + RMIN )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, the number to be converted.
!
!    real ( kind = 8 ) RMAX, RMIN, the maximum and minimum
!    values for RD.
!
!    integer ( kind = 4 ) NR, the number of allowed values for XD.
!    NR should be at least 1.
!
!  Output:
!
!    real ( kind = 8 ) RD, the corresponding discrete value.
!
  implicit none

  integer ( kind = 4 ) f
  integer ( kind = 4 ) nr
  real ( kind = 8 ) r
  real ( kind = 8 ) rd
  real ( kind = 8 ) rmax
  real ( kind = 8 ) rmin
!
!  Check for errors.
!
  if ( nr < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_TO_R8_DISCRETE - Fatal error!'
    write ( *, '(a,i8)' ) '  NR = ', nr
    write ( *, '(a)' ) '  but NR must be at least 1.'
    stop 1
  end if

  if ( nr == 1 ) then
    rd = 0.5D+00 * ( rmin + rmax )
    return
  end if

  if ( rmax == rmin ) then
    rd = rmax
    return
  end if

  f = nint ( real ( nr, kind = 8 ) * ( rmax - r ) / ( rmax - rmin ) )
  f = max ( f, 0 )
  f = min ( f, nr )

  rd = ( real (      f, kind = 8 ) * rmin   &
       + real ( nr - f, kind = 8 ) * rmax ) &
       / real ( nr,     kind = 8 )

  return
end
subroutine r8_to_dhms ( r, d, h, m, s )

!*****************************************************************************80
!
!! R8_TO_DHMS converts decimal days into days, hours, minutes, seconds.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, a decimal number representing a time
!    period measured in days.
!
!  Output:
!
!    integer ( kind = 4 ) D, H, M, S, the equivalent number of days,
!    hours, minutes and seconds.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  real ( kind = 8 ) r
  real ( kind = 8 ) r_copy
  integer ( kind = 4 ) s

  r_copy = abs ( r )

  d = int ( r_copy )

  r_copy = r_copy - d
  r_copy = 24.0D+00 * r_copy
  h = int ( r_copy )

  r_copy = r_copy - h
  r_copy = 60.0D+00 * r_copy
  m = int ( r_copy )

  r_copy = r_copy - m
  r_copy = 60.0D+00 * r_copy
  s = int ( r_copy )

  if ( r < 0.0D+00 ) then
    d = -d
    h = -h
    m = -m
    s = -s
  end if

  return
end
subroutine r8_to_i4 ( xmin, xmax, x, ixmin, ixmax, ix )

!*****************************************************************************80
!
!! R8_TO_I4 maps X in [XMIN, XMAX] to integer IX in [IXMIN, IXMAX].
!
!  Formula:
!
!    IX := IXMIN + ( IXMAX - IXMIN ) * ( X - XMIN ) / ( XMAX - XMIN )
!    IX := min ( IX, max ( IXMIN, IXMAX ) )
!    IX := max ( IX, min ( IXMIN, IXMAX ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) XMIN, XMAX, the range.  XMAX and
!    XMIN must not be equal.  It is not necessary that XMIN be less than XMAX.
!
!    real ( kind = 8 ) X, the number to be converted.
!
!    integer ( kind = 4 ) IXMIN, IXMAX, the allowed range of the output
!    variable.  IXMAX corresponds to XMAX, and IXMIN to XMIN.
!    It is not necessary that IXMIN be less than IXMAX.
!
!  Output:
!
!    integer ( kind = 4 ) IX, the value in the range [IXMIN,IXMAX] that
!    corresponds to X.
!
  implicit none

  integer ( kind = 4 ) ix
  integer ( kind = 4 ) ixmax
  integer ( kind = 4 ) ixmin
  real ( kind = 8 ) temp
  real ( kind = 8 ) x
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin

  if ( xmax == xmin ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_TO_I4 - Fatal error!'
    write ( *, '(a)' ) '  XMAX = XMIN, making a zero divisor.'
    write ( *, '(a,g14.6)' ) '  XMAX = ', xmax
    write ( *, '(a,g14.6)' ) '  XMIN = ', xmin
    stop 1
  end if

  temp = &
      ( ( xmax - x        ) * real ( ixmin, kind = 8 )  &
      + (        x - xmin ) * real ( ixmax, kind = 8 ) ) &
      / ( xmax     - xmin )

  if ( 0.0D+00 <= temp ) then
    temp = temp + 0.5D+00
  else
    temp = temp - 0.5D+00
  end if

  ix = int ( temp )

  return
end
function r8_uniform_01 ( seed )

!*****************************************************************************80
!
!! R8_UNIFORM_01 returns a unit pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer variable.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2^31 - 1 )
!      r8_uniform_01 = seed / ( 2^31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R8_UNIFORM_01
!      SEED      SEED
!
!         12345   207482415  0.096616
!     207482415  1790989824  0.833995
!    1790989824  2035175616  0.947702
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Input:
!
!    integer ( kind = 4 ) SEED, the "seed" value.
!
!  Output:
!
!    real ( kind = 8 ) R8_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
!
!    integer ( kind = 4 ) SEED, an updated seed.
!
  implicit none

  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r8_uniform_01 = real ( seed, kind = 8 ) * 4.656612875D-10

  return
end
function r8_uniform_ab ( a, b, seed )

!*****************************************************************************80
!
!! R8_UNIFORM_AB returns a scaled pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer variable.
!
!    The pseudorandom number should be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A, B, the limits of the interval.
!
!    integer ( kind = 4 ) SEED, the "seed" value.
!
!  Output:
!
!    real ( kind = 8 ) R8_UNIFORM_AB, a number strictly between A and B.
!
!    integer ( kind = 4 ) SEED, an updated seed.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r8_uniform_ab = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

  return
end
subroutine r8_unswap3 ( x, y, z )

!*****************************************************************************80
!
!! R8_UNSWAP3 unswaps three R8's.
!
!  Example:
!
!    Input:
!
!      X = 2, Y = 3, Z = 1
!
!    Output:
!
!      X = 1, Y = 2, Z = 3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, Y, Z, three values to be unswapped.
!
!  Output:
!
!    real ( kind = 8 ) X, Y, Z, the unswapped values.
!
  implicit none

  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  w = z
  z = y
  y = x
  x = w

  return
end
function r8_walsh_1d ( x, digit )

!*****************************************************************************80
!
!! R8_WALSH_1D evaluates the Walsh function.
!
!  Discussion:
!
!    Consider the binary representation of X, and number the digits
!    in descending order, from leading to lowest, with the units digit
!    being numbered 0.
!
!    The Walsh function W(J)(X) is equal to the J-th binary digit of X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 May 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, the argument of the Walsh function.
!
!    integer ( kind = 4 ) DIGIT, the index of the Walsh function.
!
!  Output:
!
!    real ( kind = 8 ) R8_WALSH_1D, the value of the Walsh function.
!
  implicit none

  integer ( kind = 4 ) digit
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_walsh_1d
  real ( kind = 8 ) x
  real ( kind = 8 ) x_copy
!
!  Hide the effect of the sign of X.
!
  x_copy = abs ( x )
!
!  If DIGIT is positive, divide by 2 DIGIT times.
!  If DIGIT is negative, multiply by 2 (-DIGIT) times.
!
  x_copy = x_copy / 2.0D+00 ** digit
!
!  Make it an integer.
!  Because it's positive, and we're using INT, we don't change the
!  units digit.
!
  n = int ( x_copy )
!
!  Is the units digit odd or even?
!
  if ( mod ( n, 2 ) == 0 ) then
    r8_walsh_1d = 0.0D+00
  else
    r8_walsh_1d = 1.0D+00
  end if

  return
end
function r8_wrap ( r, rlo, rhi )

!*****************************************************************************80
!
!! R8_WRAP forces an R8 to lie between given limits by wrapping.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!  Example:
!
!    RLO = 4.0, RHI = 8.0
!
!     R  Value
!
!    -2     8
!    -1     4
!     0     5
!     1     6
!     2     7
!     3     8
!     4     4
!     5     5
!     6     6
!     7     7
!     8     8
!     9     4
!    10     5
!    11     6
!    12     7
!    13     8
!    14     4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, a value.
!
!    real ( kind = 8 ) RLO, RHI, the desired bounds.
!
!  Output:
!
!    real ( kind = 8 ) R8_WRAP, a "wrapped" version of the value.
!
  implicit none

  real ( kind = 8 ) r
  real ( kind = 8 ) r8_wrap
  real ( kind = 8 ) rhi
  real ( kind = 8 ) rlo

  if ( r < rlo ) then
    r8_wrap = rhi - mod ( rlo - r, rhi - rlo )
  else
    r8_wrap = rlo + mod ( r - rlo, rhi - rlo )
  end if

  return
end
function r82_dist_l2 ( a1, a2 )

!*****************************************************************************80
!
!! R82_DIST_L2 returns the L2 distance between a pair of R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!    The vector L2 norm is defined as:
!
!      sqrt ( sum ( 1 <= I <= N ) A(I) * A(I) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A1(2), A2(2), the vectors.
!
!  Output:
!
!    real ( kind = 8 ) R82_DIST_L2, the L2 norm of the distance
!    between A1 and A2.
!
  implicit none

  real ( kind = 8 ) a1(2)
  real ( kind = 8 ) a2(2)
  real ( kind = 8 ) r82_dist_l2

  r82_dist_l2 = sqrt ( sum ( ( a1(1:2) - a2(1:2) ) ** 2 ) )

  return
end
function r82_eq ( a1, a2 )

!*****************************************************************************80
!
!! R82_EQ == ( A1 == A2 ) for two R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!    The comparison is lexicographic.
!
!    A1 == A2  <=>  A1(1) == A2(1) and A1(2) == A2(2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A1(2), A2(2), two R82 vectors to be compared.
!
!  Output:
!
!    logical ( kind = 4 ) R82_EQ, is TRUE if and only if A1 == A2.
!
  implicit none

  real ( kind = 8 ) a1(2)
  real ( kind = 8 ) a2(2)
  logical ( kind = 4 ) r82_eq

  if ( all ( a1(1:2) == a2(1:2) ) ) then
    r82_eq = .true.
  else
    r82_eq = .false.
  end if

  return
end
function r82_ge ( a1, a2 )

!*****************************************************************************80
!
!! R82_GE == ( A1 >= A2 ) for two R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!    The comparison is lexicographic.
!
!    A1 >= A2  <=>  A1(1) > A2(1) or ( A1(1) == A2(1) and A1(2) >= A2(2) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A1(2), A2(2), R82 vectors to be compared.
!
!  Output:
!
!    logical ( kind = 4 ) R82_GE, is TRUE if and only if A1 >= A2.
!
  implicit none

  real ( kind = 8 ) a1(2)
  real ( kind = 8 ) a2(2)
  integer ( kind = 4 ) i
  logical ( kind = 4 ) r82_ge

  r82_ge = .true.

  do i = 1, 2

    if ( a2(i) < a1(i) ) then
      r82_ge = .true.
      exit
    else if ( a1(i) < a2(i) ) then
      r82_ge = .false.
      exit
    end if

  end do

  return
end
function r82_gt ( a1, a2 )

!*****************************************************************************80
!
!! R82_GT == ( A1 > A2 ) for two R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R2, with two entries.
!
!    The comparison is lexicographic.
!
!    A1 > A2  <=>  A1(1) > A2(1) or ( A1(1) == A2(1) and A1(2) > A2(2) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A1(2), A2(2), R82 vectors to be compared.
!
!  Output:
!
!    logical ( kind = 4 ) R82_GT, is TRUE if and only if A1 > A2.
!
  implicit none

  real ( kind = 8 ) a1(2)
  real ( kind = 8 ) a2(2)
  integer ( kind = 4 ) i
  logical ( kind = 4 ) r82_gt

  r82_gt = .false.

  do i = 1, 2

    if ( a2(i) < a1(i) ) then
      r82_gt = .true.
      exit
    else if ( a1(i) < a2(i) ) then
      r82_gt = .false.
      exit
    end if

  end do

  return
end
function r82_le ( a1, a2 )

!*****************************************************************************80
!
!! R82_LE == ( A1 <= A2 ) for two R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!    The comparison is lexicographic.
!
!    A1 <= A2  <=>  A1(1) < A2(1) or ( A1(1) == A2(1) and A1(2) <= A2(2) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A1(2), A2(2), R82 vectors to be compared.
!
!  Output:
!
!    logical ( kind = 4 ) R82_LE, is TRUE if and only if A1 <= A2.
!
  implicit none

  real ( kind = 8 ) a1(2)
  real ( kind = 8 ) a2(2)
  integer ( kind = 4 ) i
  logical ( kind = 4 ) r82_le

  r82_le = .true.

  do i = 1, 2

    if ( a1(i) < a2(i) ) then
      r82_le = .true.
      exit
    else if ( a2(i) < a1(i) ) then
      r82_le = .false.
      exit
    end if

  end do

  return
end
function r82_lt ( a1, a2 )

!*****************************************************************************80
!
!! R82_LT == ( A1 < A2 ) for two R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!    The comparison is lexicographic.
!
!    A1 < A2  <=>  A1(1) < A2(1) or ( A1(1) == A2(1) and A1(2) < A2(2) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A1(2), A2(2), R82 vectors to be compared.
!
!  Output:
!
!    logical ( kind = 4 ) R82_LT, is TRUE if and only if A1 < A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a1(dim_num)
  real ( kind = 8 ) a2(dim_num)
  integer ( kind = 4 ) i
  logical ( kind = 4 ) r82_lt

  r82_lt = .false.

  do i = 1, dim_num

    if ( a1(i) < a2(i) ) then
      r82_lt = .true.
      exit
    else if ( a2(i) < a1(i) ) then
      r82_lt = .false.
      exit
    end if

  end do

  return
end
function r82_ne ( a1, a2 )

!*****************************************************************************80
!
!! R82_NE == ( A1 /= A2 ) for two R82's.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!    The comparison is lexicographic.
!
!    A1 /= A2  <=>  A1(1) /= A2(1) or A1(2) /= A2(2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A1(2), A2(2), R82 vectors to be compared.
!
!  Output:
!
!    logical ( kind = 4 ) R82_NE, is TRUE if and only if A1 /= A2.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a1(dim_num)
  real ( kind = 8 ) a2(dim_num)
  logical ( kind = 4 ) r82_ne

  if ( any ( a1(1:dim_num) /= a2(1:dim_num) ) ) then
    r82_ne = .true.
  else
    r82_ne = .false.
  end if

  return
end
function r82_norm ( a )

!*****************************************************************************80
!
!! R82_NORM returns the Euclidean norm of an R82.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A(2), the vector.
!
!  Output:
!
!    real ( kind = 8 ) R82_NORM, the norm.
!
  implicit none

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) r82_norm

  r82_norm = sqrt ( a(1) * a(1) + a(2) * a(2) )

  return
end
subroutine r82_normalize ( a )

!*****************************************************************************80
!
!! R82_NORMALIZE Euclidean normalizes an R82.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A(2), the vector to be normalized.
!
!  Output:
!
!    real ( kind = 8 ) A(2), the normalized vector.
!
  implicit none

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) norm

  norm = sqrt ( a(1) * a(1) + a(2) * a(2) )

  if ( norm /= 0.0D+00 ) then
    a(1:2) = a(1:2) / norm
  end if

  return
end
subroutine r82_print ( a, title )

!*****************************************************************************80
!
!! R82_PRINT prints an R82.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!    A format is used which suggests a coordinate pair:
!
!  Example:
!
!    Center : ( 1.23, 7.45 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A(2), the coordinates of the vector.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  real ( kind = 8 ) a(2)
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '( 2x, a, a4, g14.6, a1, g14.6, a1 )' ) &
      trim ( title ), ' : (', a(1), ',', a(2), ')'
  else
    write ( *, '( 2x, a1, g14.6, a1, g14.6, a1 )' ) '(', a(1), ',', a(2), ')'

  end if

  return
end
subroutine r82_swap ( x, y )

!*****************************************************************************80
!
!! R82_SWAP swaps two R82 values.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X(2), Y(2), vectors to be interchanged.
!
!  Output:
!
!    real ( kind = 8 ) X(2), Y(2), the interchanged vectors.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) x(dim_num)
  real ( kind = 8 ) y(dim_num)
  real ( kind = 8 ) z(dim_num)

  z(1:dim_num) = x(1:dim_num)
  x(1:dim_num) = y(1:dim_num)
  y(1:dim_num) = z(1:dim_num)

  return
end
subroutine r82_uniform_ab ( b, c, seed, a )

!*****************************************************************************80
!
!! R82_UNIFORM_AB returns a random R82 value in a given range.
!
!  Discussion:
!
!    An R82 is a vector of type R8, with two entries.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!
!  Input:
!
!    real ( kind = 8 ) B, C, the minimum and maximum values.
!
!    integer ( kind = 4 ) SEED, a seed for the random number generator.
!
!  Output:
!
!    real ( kind = 8 ) A(2), the randomly chosen value.
!
!    integer ( kind = 4 ) SEED, an updated seed.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num)
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8_uniform_ab
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed

  do i = 1, dim_num
    a(i) = r8_uniform_ab ( b, c, seed )
  end do

  return
end
subroutine r82col_print_part ( n, a, max_print, title )

!*****************************************************************************80
!
!! R82COL_PRINT_PART prints "part" of an R82COL.
!
!  Discussion:
!
!    An R82COL is an (N,2) array of R8's.
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of the vector.
!
!    real ( kind = 8 ) A(N,2), the vector to be printed.
!
!    integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * ) title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(i,1:2)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(i,1:2)
    end do
    write ( *, '(a)' ) '  ........  ..............  ..............'
    i = n
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(i,1:2)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(i,1:2)
    end do
    i = max_print
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,a)' ) i, ':', a(i,1:2), &
      '...more entries...'

  end if

  return
end
subroutine r82row_max ( n, a, amax )

!*****************************************************************************80
!
!! R82ROW_MAX returns the maximum value in an R82ROW.
!
!  Discussion:
!
!    An R82ROW is a (2,N) array of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2006
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(2,N), the array.
!
!  Output:
!
!    real ( kind = 8 ) AMAX(2); the largest entries in each row.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2,n)
  real ( kind = 8 ) amax(2)

  amax(1) = maxval ( a(1,1:n) )
  amax(2) = maxval ( a(2,1:n) )

  return
end
subroutine r82row_min ( n, a, amin )

!*****************************************************************************80
!
!! R82ROW_MIN returns the minimum value in an R82ROW.
!
!  Discussion:
!
!    An R82ROW is a (2,N) array of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2006
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(2,N), the array.
!
!  Output:
!
!    real ( kind = 8 ) AMIN(2); the smallest entries in each row.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2,n)
  real ( kind = 8 ) amin(2)

  amin(1) = minval ( a(1,1:n) )
  amin(2) = minval ( a(2,1:n) )

  return
end
subroutine r82row_order_type ( n, a, order )

!*****************************************************************************80
!
!! R82ROW_ORDER_TYPE finds the order type of an R82ROW.
!
!  Discussion:
!
!    An R82ROW is a (2,N) array of R8's.
!
!    The dictionary or lexicographic ordering is used.
!
!    (X1,Y1) < (X2,Y2)  <=>  X1 < X2 or ( X1 = X2 and Y1 < Y2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of the array.
!
!    real ( kind = 8 ) A(2,N), the array to be checked.
!
!  Output:
!
!    integer ( kind = 4 ) ORDER, order indicator:
!    -1, no discernable order;
!    0, all entries are equal;
!    1, ascending order;
!    2, strictly ascending order;
!    3, descending order;
!    4, strictly descending order.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order
!
!  Search for the first value not equal to A(1,1).
!
  i = 1

  do

    i = i + 1

    if ( n < i ) then
      order = 0
      return
    end if

    if ( &
         a(1,1) <  a(1,i) .or. &
       ( a(1,1) == a(1,i) .and. a(2,1) < a(2,i) ) &
    ) then

      if ( i == 2 ) then
        order = 2
      else
        order = 1
      end if

      exit

    else if ( &
        a(1,i) <  a(1,1)  .or. &
      ( a(1,i) == a(1,1) .and. a(2,i) < a(2,1) ) &
    ) then

      if ( i == 2 ) then
        order = 4
      else
        order = 3
      end if

      exit

    end if

  end do
!
!  Now we have a "direction".  Examine subsequent entries.
!
  do

    i = i + 1
    if ( n < i ) then
      exit
    end if

    if ( order == 1 ) then

      if ( &
          a(1,i) <  a(1,i-1) .or. &
        ( a(1,i) == a(1,i-1) .and. a(2,i) < a(2,i-1) ) &
      ) then
        order = -1
        exit
      end if

    else if ( order == 2 ) then

      if ( &
          a(1,i) <  a(1,i-1) .or. &
        ( a(1,i) == a(1,i-1) .and. a(2,i) < a(2,i-1) ) &
      ) then
        order = -1
        exit
      else if ( &
         a(1,i) == a(1,i-1) .and. a(2,i) == a(2,i-1) ) then
        order = 1
      end if

    else if ( order == 3 ) then

      if ( &
          a(1,i-1) <  a(1,i) .or. &
        ( a(1,i-1) == a(1,i) .and. a(2,i-1) < a(2,i) ) &
      ) then
        order = -1
        exit
      end if

    else if ( order == 4 ) then

      if ( &
          a(1,i-1) <  a(1,i) .or. &
        ( a(1,i-1) == a(1,i) .and. a(2,i-1) < a(2,i) ) &
      ) then
        order = -1
        exit
      else if ( a(1,i) == a(1,i-1) .and. a(2,i) == a(2,i-1) ) then
        order = 3
      end if

    end if

  end do

  return
end
subroutine r82row_part_quick_a ( n, a, l, r )

!*****************************************************************************80
!
!! R82ROW_PART_QUICK_A reorders an R82ROW as part of a quick sort.
!
!  Discussion:
!
!    An R82ROW is a (2,N) array of R8's.
!
!    The routine reorders the entries of A.  Using A(1:2,1) as a
!    key, all entries of A that are less than or equal to the key will
!    precede the key, which precedes all entries that are greater than the key.
!
!  Example:
!
!    Input:
!
!      N = 8
!
!      A = ( (2,4), (8,8), (6,2), (0,2), (10,6), (10,0), (0,6), (4,8) )
!
!    Output:
!
!      L = 2, R = 4
!
!      A = ( (0,2), (0,6), (2,4), (8,8), (6,2), (10,6), (10,0), (4,8) )
!             -----------          ----------------------------------
!             LEFT          KEY    RIGHT
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of A.
!
!    real ( kind = 8 ) A(2,N), the array to be checked.
!
!  Output:
!
!    real ( kind = 8 ) A(2,N), the reordered array.
!
!    integer ( kind = 4 ) L, R, the indices of A that define the three
!    segments.  Let KEY = the input value of A(1:2,1).  Then
!    I <= L                 A(1:2,I) < KEY;
!         L < I < R         A(1:2,I) = KEY;
!                 R <= I    KEY < A(1:2,I).
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) key(dim_num)
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) r
  logical ( kind = 4 ) r8vec_eq
  logical ( kind = 4 ) r8vec_gt
  logical ( kind = 4 ) r8vec_lt

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R82ROW_PART_QUICK_A - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    write ( *, '(a,i8)' ) '  N = ', n
    stop 1
  else if ( n == 1 ) then
    l = 0
    r = 2
    return
  end if

  key(1:dim_num) = a(1:dim_num,1)
  m = 1
!
!  The elements of unknown size have indices between L+1 and R-1.
!
  l = 1
  r = n + 1

  do i = 2, n

    if ( r8vec_gt ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) then
      r = r - 1
      call r8vec_swap ( dim_num, a(1:dim_num,r), a(1:dim_num,l+1) )
    else if ( r8vec_eq ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) then
      m = m + 1
      call r8vec_swap ( dim_num, a(1:dim_num,m), a(1:dim_num,l+1) )
      l = l + 1
    else if ( r8vec_lt ( dim_num, a(1:dim_num,l+1), key(1:dim_num) ) ) then
      l = l + 1
    end if

  end do
!
!  Now shift small elements to the left, and KEY elements to center.
!
  do i = 1, l - m
    a(1:dim_num,i) = a(1:dim_num,i+m)
  end do

  l = l - m

  do i = 1, dim_num
    a(i,l+1:l+m) = key(i)
  end do

  return
end
subroutine r82row_permute ( n, p, a )

!*****************************************************************************80
!
!! R82ROW_PERMUTE permutes an R82ROW in place.
!
!  Discussion:
!
!    An R82ROW is a (2,N) array of R8's.
!
!    The same logic can be used to permute an array of objects of any
!    arithmetic type, or an array of objects of any complexity.  The only
!    temporary storage required is enough to store a single object.  The number
!    of data movements made is N + the number of cycles of order 2 or more,
!    which is never more than N + N/2.
!
!  Example:
!
!    Input:
!
!      N = 5
!      P = (   2,    4,    5,    1,    3 )
!      A = ( 1.0,  2.0,  3.0,  4.0,  5.0 )
!          (11.0, 22.0, 33.0, 44.0, 55.0 )
!
!    Output:
!
!      A    = (  2.0,  4.0,  5.0,  1.0,  3.0 )
!             ( 22.0, 44.0, 55.0, 11.0, 33.0 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of objects.
!
!    integer ( kind = 4 ) P(N), the permutation.  P(I) = J means
!    that the I-th element of the output array should be the J-th
!    element of the input array.
!
!    real ( kind = 8 ) A(2,N), the array to be permuted.
!
!  Output:
!
!    real ( kind = 8 ) A(2,N), the permuted array.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(2,n)
  real ( kind = 8 ) a_temp(2)
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) istart
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) perm1_check

  ierror = perm1_check ( n, p )

  if ( ierror .ne. 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'R82ROW_PERMUTE - Fatal error!'
    write ( *, '(a)' ) '  PERM1_CHECK returned error.'
    stop 1
  end if
!
!  Search for the next element of the permutation that has not been used.
!
  do istart = 1, n

    if ( p(istart) < 0 ) then

    else if ( p(istart) == istart ) then

      p(istart) = - p(istart)

    else

      a_temp(1:2) = a(1:2,istart)
      iget = istart
!
!  Copy the new value into the vacated entry.
!
      do

        iput = iget
        iget = p(iget)

        p(iput) = - p(iput)

        if ( iget < 1 .or. n < iget ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R82ROW_PERMUTE - Fatal error!'
          write ( *, '(a)' ) '  A permutation index is out of range.'
          write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
          stop 1
        end if

        if ( iget == istart ) then
          a(1:2,iput) = a_temp(1:2)
          exit
        end if

        a(1:2,iput) = a(1:2,iget)

      end do

    end if

  end do
!
!  Restore the signs of the entries.
!
  p(1:n) = - p(1:n)

  return
end
subroutine r82row_print ( n, a, title )

!*****************************************************************************80
!
!! R82ROW_PRINT prints an R82ROW.
!
!  Discussion:
!
!    An R82ROW is a (2,N) array of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of components of the vector.
!
!    real ( kind = 8 ) A(2,N), the R82 vector to be printed.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num,n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,(5g14.6))' ) i, a(1:dim_num,i)
  end do

  return
end
subroutine r82row_print_part ( n, a, max_print, title )

!*****************************************************************************80
!
!! R82ROW_PRINT_PART prints "part" of an R82ROW.
!
!  Discussion:
!
!    An R82ROW is a (2,N) array of R8's.
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of the vector.
!
!    real ( kind = 8 ) A(2,N), the vector to be printed.
!
!    integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(2,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * ) title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(1:2,i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(1:2,i)
    end do
    write ( *, '(a)' ) '  ........  ..............  ..............'
    i = n
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(1:2,i)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a(1:2,i)
    end do
    i = max_print
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,a)' ) i, ':', a(1:2,i), &
      '...more entries...'

  end if

  return
end
subroutine r82row_sort_heap_index_a ( n, a, indx )

!*****************************************************************************80
!
!! R82ROW_SORT_HEAP_INDEX_A ascending index heaps an R82ROW.
!
!  Discussion:
!
!    An R82ROW is a (2,N) array of R8's.
!
!    The sorting is not actually carried out.  Rather an index array is
!    created which defines the sorting.  This array may be used to sort
!    or index the array, or to sort or index related arrays keyed on the
!    original array.
!
!    Once the index array is computed, the sorting can be carried out
!    "implicitly:
!
!      A(1:2,INDX(1:N)) is sorted,
!
!    or explicitly, by the call
!
!      call r82row_permute ( n, indx, a )
!
!    after which A(1:2,I), I = 1 to N is sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(2,N), an array to be index-sorted.
!
!  Output:
!
!    integer ( kind = 4 ) INDX(N), the sort index.  The
!    I-th element of the sorted array is A(1:2,INDX(I)).
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num,n)
  real ( kind = 8 ) aval(dim_num)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indxt
  integer ( kind = 4 ) ir
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l

  if ( n < 1 ) then
    return
  end if

  do i = 1, n
    indx(i) = i
  end do

  if ( n == 1 ) then
    return
  end if

  l = n / 2 + 1
  ir = n

  do

    if ( 1 < l ) then

      l = l - 1
      indxt = indx(l)
      aval(1:dim_num) = a(1:dim_num,indxt)

    else

      indxt = indx(ir)
      aval(1:dim_num) = a(1:dim_num,indxt)
      indx(ir) = indx(1)
      ir = ir - 1

      if ( ir == 1 ) then
        indx(1) = indxt
        exit
      end if

    end if

    i = l
    j = l + l

    do while ( j <= ir )

      if ( j < ir ) then
        if (   a(1,indx(j)) <  a(1,indx(j+1)) .or. &
             ( a(1,indx(j)) == a(1,indx(j+1)) .and. &
               a(2,indx(j)) <  a(2,indx(j+1)) ) ) then
          j = j + 1
        end if
      end if

      if (   aval(1) <  a(1,indx(j)) .or. &
           ( aval(1) == a(1,indx(j)) .and. &
             aval(2) <  a(2,indx(j)) ) ) then
        indx(i) = indx(j)
        i = j
        j = j + j
      else
        j = ir + 1
      end if

    end do

    indx(i) = indxt

  end do

  return
end
subroutine r82row_sort_quick_a ( n, a )

!*****************************************************************************80
!
!! R82ROW_SORT_QUICK_A ascending sorts an R82ROW using quick sort.
!
!  Discussion:
!
!    An R82ROW is a (2,N) array of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(2,N), the array to be sorted.
!
!  Output:
!
!    real ( kind = 8 ) A(2,N), the sorted array.
!
  implicit none

  integer ( kind = 4 ), parameter :: level_max = 30
  integer ( kind = 4 ) n
  integer ( kind = 4 ), parameter :: dim_num = 2

  real ( kind = 8 ) a(dim_num,n)
  integer ( kind = 4 ) base
  integer ( kind = 4 ) l_segment
  integer ( kind = 4 ) level
  integer ( kind = 4 ) n_segment
  integer ( kind = 4 ) rsave(level_max)
  integer ( kind = 4 ) r_segment

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R82ROW_SORT_QUICK_A - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    write ( *, '(a,i8)' ) '  N = ', n
    stop 1
  else if ( n == 1 ) then
    return
  end if

  level = 1
  rsave(level) = n + 1
  base = 1
  n_segment = n

  do
!
!  Partition the segment.
!
    call r82row_part_quick_a ( n_segment, a(1,base), l_segment, r_segment )
!
!  If the left segment has more than one element, we need to partition it.
!
    if ( 1 < l_segment ) then

      if ( level_max < level ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R82ROW_SORT_QUICK_A - Fatal error!'
        write ( *, '(a,i8)' ) '  Exceeding recursion maximum of ', level_max
        stop 1
      end if

      level = level + 1
      n_segment = l_segment
      rsave(level) = r_segment + base - 1
!
!  The left segment and the middle segment are sorted.
!  Must the right segment be partitioned?
!
    else if ( r_segment < n_segment ) then

      n_segment = n_segment + 1 - r_segment
      base = base + r_segment - 1
!
!  Otherwise, we back up a level if there is an earlier one.
!
    else

      do

        if ( level <= 1 ) then
          return
        end if

        base = rsave(level)
        n_segment = rsave(level-1) - rsave(level)
        level = level - 1

        if ( 0 < n_segment ) then
          exit
        end if

      end do

    end if

  end do

  return
end
function r83_norm ( x, y, z )

!*****************************************************************************80
!
!! R83_NORM returns the Euclidean norm of an R83.
!
!  Discussion:
!
!    An R83 is a vector of 3 R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, Y, Z, the vector.
!
!  Output:
!
!    real ( kind = 8 ) R83_NORM, the norm of the vector.
!
  implicit none

  real ( kind = 8 ) r83_norm
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  r83_norm = sqrt ( x * x + y * y + z * z )

  return
end
subroutine r83_normalize ( x, y, z )

!*****************************************************************************80
!
!! R83_NORMALIZE normalizes an R83.
!
!  Discussion:
!
!    An R83 is a vector of 3 R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, Y, Z, the components of the vector.
!
!  Output:
!
!    real ( kind = 8 ) X, Y, Z, the components of the normalized vector.
!
  implicit none

  real ( kind = 8 ) norm
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  norm = sqrt ( x * x + y * y + z * z )

  if ( norm /= 0.0D+00 ) then
    x = x / norm
    y = y / norm
    z = z / norm
  end if

  return
end
subroutine r83_print ( x, y, z, title )

!*****************************************************************************80
!
!! R83_PRINT prints an R83.
!
!  Discussion:
!
!    An R83 is a vector of 3 R8's.
!
!    A format is used which suggests a coordinate triple:
!
!  Example:
!
!    Center : ( 1.23, 7.45, -1.45 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X, Y, Z, the coordinates of the vector.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  character ( len = * ) title
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  if ( 0 < len_trim ( title ) ) then
    write ( *, '( 2x, a, a4, g14.6, a1, g14.6, a1, g14.6, a1 )' ) &
      trim ( title ), ' : (', x, ',', y, ',', z, ')'
  else
    write ( *, '( 2x, a1, g14.6, a1, g14.6, a1, g14.6, a1 )' ) &
      '(', x, ',', y, ',', z, ')'
  end if

  return
end
subroutine r83_swap ( x, y )

!*****************************************************************************80
!
!! R83_SWAP swaps two R83's.
!
!  Discussion:
!
!    An R83 is a vector of 3 R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X(3), Y(3), the vectors to be interchanged.
!
!  Output:
!
!    real ( kind = 8 ) X(3), Y(3), the interchanged vectors.
!
  implicit none

  integer ( kind = 4 ), parameter :: dim_num = 3

  real ( kind = 8 ) x(dim_num)
  real ( kind = 8 ) y(dim_num)
  real ( kind = 8 ) z(dim_num)

  z(1:dim_num) = x(1:dim_num)
  x(1:dim_num) = y(1:dim_num)
  y(1:dim_num) = z(1:dim_num)

  return
end
subroutine r83col_print_part ( n, a, max_print, title )

!*****************************************************************************80
!
!! R83COL_PRINT_PART prints "part" of an R83COL.
!
!  Discussion:
!
!    An R83COL is an (N,3) array of R8's.
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of the vector.
!
!    real ( kind = 8 ) A(N,3), the vector to be printed.
!
!    integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,3)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * ) title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(i,1:3)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(i,1:3)
    end do
    write ( *, '(a)' ) &
      '  ........  ..............  ..............  ..............'
    i = n
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(i,1:3)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(i,1:3)
    end do
    i = max_print
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6,2x,a)' ) i, ':', a(i,1:3), &
      '...more entries...'

  end if

  return
end
subroutine r83row_max ( n, a, amax )

!*****************************************************************************80
!
!! R83ROW_MAX returns the maximum value in an R83ROW.
!
!  Discussion:
!
!    An R83ROW is a (3,N) array of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(3,N), the array.
!
!  Output:
!
!    real ( kind = 8 ) AMAX(3); the largest entries in each row.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(3,n)
  real ( kind = 8 ) amax(3)
  integer ( kind = 4 ) i

  do i = 1, 3
    amax(i) = maxval ( a(i,1:n) )
  end do

  return
end
subroutine r83row_min ( n, a, amin )

!*****************************************************************************80
!
!! R83ROW_MIN returns the minimum value in an R83ROW.
!
!  Discussion:
!
!    An R83ROW is a (3,N) array of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(3,N), the array.
!
!  Output:
!
!    real ( kind = 8 ) AMIN(3); the smallest entries in each row.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(3,n)
  real ( kind = 8 ) amin(3)
  integer ( kind = 4 ) i

  do i = 1, 3
    amin(i) = minval ( a(i,1:n) )
  end do

  return
end
subroutine r83row_normalize ( n, x )

!*****************************************************************************80
!
!! R83ROW_NORMALIZE normalizes each R83 in an R83ROW.
!
!  Discussion:
!
!    An R83ROW is a (3,N) array of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of R83 vectors.
!
!    real ( kind = 8 ) X(3,N), the N R83 vectors.
!
!  Output:
!
!    real ( kind = 8 ) X(3,N), the normalized vectors..
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) norm
  real ( kind = 8 ) x(3,n)

  do i = 1, n

    norm = sqrt ( sum ( x(1:3,i) ** 2 ) )

    if ( norm /= 0.0D+00 ) then
      x(1:3,i) = x(1:3,i) / norm
    end if

  end do

  return
end
subroutine r83row_print_part ( n, a, max_print, title )

!*****************************************************************************80
!
!! R83ROW_PRINT_PART prints "part" of an R83ROW.
!
!  Discussion:
!
!    An R83ROW is a (3,N) array of R8's.
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of the vector.
!
!    real ( kind = 8 ) A(3,N), the vector to be printed.
!
!    integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(3,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * ) title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)
    end do
    write ( *, '(a)' ) &
      '  ........  ..............  ..............  ..............'
    i = n
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)
    end do
    i = max_print
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6,2x,a)' ) i, ':', a(1:3,i), &
      '...more entries...'

  end if

  return
end
subroutine r84_normalize ( v )

!*****************************************************************************80
!
!! R84_NORMALIZE normalizes an R84.
!
!  Discussion:
!
!    An R84 is a vector of four R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) V(4), the components of the vector.
!
!  Output:
!
!    real ( kind = 8 ) V(4), the normalized vector.
!
  implicit none

  real ( kind = 8 ) norm
  real ( kind = 8 ) v(4)

  norm = sqrt ( sum ( v(1:4) ** 2 ) )

  if ( norm /= 0.0D+00 ) then
    v(1:4) = v(1:4) / norm
  end if

  return
end
subroutine r8block_expand_linear ( l, m, n, x, lfat, mfat, nfat, xfat )

!*****************************************************************************80
!
!! R8BLOCK_EXPAND_LINEAR linearly interpolates new data into an R8BLOCK.
!
!  Discussion:
!
!    An R8BLOCK is a 3D array of R8 values.
!
!    In this routine, the expansion is specified by giving the number
!    of intermediate values to generate between each pair of original
!    data rows and columns.
!
!    The interpolation is not actually linear.  It uses the functions
!
!      1, x, y, z, xy, xz, yz, xyz.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 October 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) L, M, N, the dimensions of the input data.
!
!    real ( kind = 8 ) X(L,M,N), the original data.
!
!    integer ( kind = 4 ) LFAT, MFAT, NFAT, the number of data values
!    to interpolate original data values in the first, second and third
!    dimensions.
!
!  Output:
!
!    real ( kind = 8 ) XFAT(L2,M2,N2), the fattened data, where
!    L2 = (L-1)*(LFAT+1)+1,
!    M2 = (M-1)*(MFAT+1)+1,
!    N2 = (N-1)*(NFAT+1)+1.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) lfat
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mfat
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nfat

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) iii
  integer ( kind = 4 ) ip1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jj
  integer ( kind = 4 ) jjj
  integer ( kind = 4 ) jp1
  integer ( kind = 4 ) k
  integer ( kind = 4 ) khi
  integer ( kind = 4 ) kk
  integer ( kind = 4 ) kkk
  integer ( kind = 4 ) kp1
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) x(l,m,n)
  real ( kind = 8 ) x000
  real ( kind = 8 ) x001
  real ( kind = 8 ) x010
  real ( kind = 8 ) x011
  real ( kind = 8 ) x100
  real ( kind = 8 ) x101
  real ( kind = 8 ) x110
  real ( kind = 8 ) x111
  real ( kind = 8 ) xfat((l-1)*(lfat+1)+1,(m-1)*(mfat+1)+1,(n-1)*(nfat+1)+1)

  do i = 1, l

    if ( i < l ) then
      ihi = lfat
    else
      ihi = 0
    end if

    do j = 1, m

      if ( j < m ) then
        jhi = mfat
      else
        jhi = 0
      end if

      do k = 1, n

        if ( k < n ) then
          khi = nfat
        else
          khi = 0
        end if

        if ( i < l ) then
          ip1 = i + 1
        else
          ip1 = i
        end if

        if ( j < m ) then
          jp1 = j + 1
        else
          jp1 = j
        end if

        if ( k < n ) then
          kp1 = k + 1
        else
          kp1 = k
        end if

        x000 = x(i,j,k)
        x001 = x(i,j,kp1)
        x100 = x(ip1,j,k)
        x101 = x(ip1,j,kp1)
        x010 = x(i,jp1,k)
        x011 = x(i,jp1,kp1)
        x110 = x(ip1,jp1,k)
        x111 = x(ip1,jp1,kp1)

        do ii = 0, ihi

          r = real ( ii,      kind = 8 ) &
            / real ( ihi + 1, kind = 8 )

          do jj = 0, jhi

            s = real ( jj,      kind = 8 ) &
              / real ( jhi + 1, kind = 8 )

            do kk = 0, khi

              t = real ( kk,      kind = 8 ) &
                / real ( khi + 1, kind = 8 )

              iii = 1 + ( i - 1 ) * ( lfat + 1 ) + ii
              jjj = 1 + ( j - 1 ) * ( mfat + 1 ) + jj
              kkk = 1 + ( k - 1 ) * ( nfat + 1 ) + kk

              xfat(iii,jjj,kkk) = &
                  x000 * ( 1.0D+00 - r ) * ( 1.0D+00 - s ) * ( 1.0D+00 - t ) &
                + x001 * ( 1.0D+00 - r ) * ( 1.0D+00 - s ) * (           t ) &
                + x010 * ( 1.0D+00 - r ) * (           s ) * ( 1.0D+00 - t ) &
                + x011 * ( 1.0D+00 - r ) * (           s ) * (           t ) &
                + x100 * (           r ) * ( 1.0D+00 - s ) * ( 1.0D+00 - t ) &
                + x101 * (           r ) * ( 1.0D+00 - s ) * (           t ) &
                + x110 * (           r ) * (           s ) * ( 1.0D+00 - t ) &
                + x111 * (           r ) * (           s ) * (           t )

            end do

          end do

        end do

      end do

    end do

  end do

  return
end
subroutine r8block_print ( l, m, n, a, title )

!*****************************************************************************80
!
!! R8BLOCK_PRINT prints an R8BLOCK.
!
!  Discussion:
!
!    An R8BLOCK is a 3D array of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) L, M, N, the dimensions of the block.
!
!    real ( kind = 8 ) A(L,M,N), the matrix to be printed.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(l,m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) k
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  do k = 1, n

    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  K = ', k

    do jlo = 1, m, 5
      jhi = min ( jlo + 4, m )
      write ( *, '(a)' ) ' '
      write ( *, '(10x,5(i8,6x))' ) (j, j = jlo, jhi )
      write ( *, '(a)' ) ' '
      do i = 1, l
        write ( *, '(2x,i8,5g14.6)' ) i, a(i,jlo:jhi,k)
      end do
    end do

  end do

  return
end
subroutine r8cmat_print ( lda, m, n, a, title )

!*****************************************************************************80
!
!! R8CMAT_PRINT prints an R8CMAT.
!
!  Discussion:
!
!    An R8CMAT is an MxN array of R8's, stored with a leading dimension
!    of LD, and hence accessed either as a double indexed array:
!      (I,J) -> (I,J) 
!    or as a vector:
!      (I,J) -> (I+J*LD).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(LDA,N), the M by N matrix.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(lda,n)
  character ( len = * ) title

  call r8cmat_print_some ( lda, m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8cmat_print_some ( lda, m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8CMAT_PRINT_SOME prints some of an R8CMAT.
!
!  Discussion:
!
!    An R8CMAT is an MxN array of R8's, stored with a leading dimension
!    of LD, and hence accessed either as a double indexed array:
!      (I,J) -> (I,J) 
!    or as a vector:
!      (I,J) -> (I+J*LD).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) LDA, the leading dimension of A.
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) A(LDA,N), the M by N matrix.
!
!    integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(lda,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8,6x)' ) j
    end do

    write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        if ( a(i,j) == real ( int ( a(i,j) ), kind = 8 ) ) then
          write ( ctemp(j2), '(f8.0,6x)' ) a(i,j)
        else
          write ( ctemp(j2), '(g14.6)' ) a(i,j)
        end if

      end do

      write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

    end do

  end do

  return
end
subroutine r8cmat_to_r8mat ( lda, m, n, a1, a2 )

!*****************************************************************************80
!
!! R8CMAT_TO_R8MAT transfers data from an R8CMAT to an R8MAT.
!
!  Discussion:
!
!    An R8CMAT is an MxN array of R8's, stored with a leading dimension LD,
!    accessible as a vector:
!      (I,J) -> (I+J*LD).
!    or as a doubly-dimensioned array, if declared A(LD,N):
!      (I,J) -> A(I,J)
!
!    An R8MAT is an MxN array of R8's, 
!    accessible as a vector:
!      (I,J) -> (I+J*M).
!    or as a doubly-dimensioned array, if declared A(M,N):
!      (I,J) -> A(I,J)
!      
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 March 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) LDA, the leading dimension of A1.
!
!    integer ( kind = 4 ) M, the number of rows of data.
!    M <= LDA.
!
!    integer ( kind = 4 ) N, the number of columns of data.
!
!    real ( kind = 8 ) A1(LDA,N), the matrix to be copied.
!
!  Output:
!
!    real ( kind = 8 ) A2(M,N), a copy of the
!    information in the MxN submatrix of A1.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(lda,n)
  real ( kind = 8 ) a2(m,n)

  a2(1:m,1:n) = a1(1:m,1:n)
 
  return
end
subroutine r8int_to_r8int ( rmin, rmax, r, r2min, r2max, r2 )

!*****************************************************************************80
!
!! R8INT_TO_R8INT maps one R8INT to another.
!
!  Discussion:
!
!    The formula used is
!
!      R2 := R2MIN + ( R2MAX - R2MIN ) * ( R - RMIN ) / ( RMAX - RMIN )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) RMIN, RMAX, the first range.
!
!    real ( kind = 8 ) R, the number to be converted.
!
!    real ( kind = 8 ) R2MAX, R2MIN, the second range.
!
!  Output:
!
!    real ( kind = 8 ) R2, the corresponding value in
!    the range [R2MIN,R2MAX].
!
  implicit none

  real ( kind = 8 ) r
  real ( kind = 8 ) rmax
  real ( kind = 8 ) rmin
  real ( kind = 8 ) r2
  real ( kind = 8 ) r2max
  real ( kind = 8 ) r2min

  if ( rmax == rmin ) then

    r2 = ( r2max + r2min ) / 2.0D+00

  else

    r2 = ( ( ( rmax - r        ) * r2min   &
           + (        r - rmin ) * r2max ) &
           / ( rmax     - rmin ) )

  end if

  return
end
subroutine r8int_to_i4int ( rmin, rmax, r, imin, imax, i )

!*****************************************************************************80
!
!! R8INT_TO_I4INT maps an R8INT to an integer interval.
!
!  Discussion:
!
!    The formula used is
!
!      I := IMIN + ( IMAX - IMIN ) * ( R - RMIN ) / ( RMAX - RMIN )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) RMIN, RMAX, the range.
!
!    real ( kind = 8 ) R, the number to be converted.
!
!    integer ( kind = 4 ) IMAX, IMIN, the integer range.
!
!  Output:
!
!    integer ( kind = 4 ) I, the corresponding value in the
!    range [IMIN,IMAX].
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) imax
  integer ( kind = 4 ) imin
  real ( kind = 8 ) r
  real ( kind = 8 ) rmax
  real ( kind = 8 ) rmin

  if ( rmax == rmin ) then

    i = ( imax + imin ) / 2

  else

    i = nint ( &
      ( ( rmax - r        ) * real ( imin, kind = 8 )   &
      + (        r - rmin ) * real ( imax, kind = 8 ) ) &
      / ( rmax     - rmin ) )

  end if

  return
end
subroutine r8mat_add ( m, n, alpha, a, beta, b, c )

!*****************************************************************************80
!
!! R8MAT_ADD computes C = alpha * A + beta * B for R8MAT's.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) ALPHA, the multiplier for A.
!
!    real ( kind = 8 ) A(M,N), the first matrix.
!
!    real ( kind = 8 ) BETA, the multiplier for A.
!
!    real ( kind = 8 ) B(M,N), the second matrix.
!
!  Output:
!
!    real ( kind = 8 ) C(M,N), the sum of alpha*A+beta*B.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(m,n)
  real ( kind = 8 ) beta
  real ( kind = 8 ) c(m,n)

  c(1:m,1:n) = alpha * a(1:m,1:n) + beta * b(1:m,1:n)

  return
end
function r8mat_amax ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_AMAX returns the maximum absolute value entry of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_AMAX, the maximum absolute value 
!    entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_amax

  r8mat_amax = maxval ( abs ( a(1:m,1:n) ) )

  return
end
subroutine r8mat_border_add ( m, n, table, table2 )

!*****************************************************************************80
!
!! R8MAT_BORDER_ADD adds a "border" to an R8MAT.
!
!  Discussion:
!
!    We suppose the input data gives values of a quantity on nodes
!    in the interior of a 2D grid, and we wish to create a new table
!    with additional positions for the nodes that would be on the
!    border of the 2D grid.
!
!                  0 0 0 0 0 0
!      * * * *     0 * * * * 0
!      * * * * --> 0 * * * * 0
!      * * * *     0 * * * * 0
!                  0 0 0 0 0 0
!
!    The illustration suggests the situation in which a 3 by 4 array
!    is and a 5 by 6 array is to be output.
!
!    The old data is shifted to its correct positions in the new array.
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the spatial dimension.
!
!    integer ( kind = 4 ) N, the number of points.
!
!    real ( kind = 8 ) TABLE(M,N), the table data.
!
!  Output:
!
!    real ( kind = 8 ) TABLE2(M+2,N+2), the augmented table data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) table(m,n)
  real ( kind = 8 ) table2(m+2,n+2)

  table2(1,1:n+2) = 0.0D+00
  table2(m+2,1:n+2) = 0.0D+00
  table2(2:m+1,1) = 0.0D+00
  table2(2:m+1,n+2) = 0.0D+00

  table2(2:m+1,2:n+1) = table(1:m,1:n)

  return
end
subroutine r8mat_border_cut ( m, n, table, table2 )

!*****************************************************************************80
!
!! R8MAT_BORDER_CUT cuts the "border" of an R8MAT.
!
!  Discussion:
!
!    We suppose the input data gives values of a quantity on nodes
!    on a 2D grid, and we wish to create a new table corresponding only
!    to those nodes in the interior of the 2D grid.
!
!      0 0 0 0 0 0
!      0 * * * * 0    * * * *
!      0 * * * * 0 -> * * * *
!      0 * * * * 0    * * * *
!      0 0 0 0 0 0
!
!    The illustration suggests the situation in which a 5 by 6 array
!    is and a 3 by 4 array is to be output.
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the spatial dimension.
!
!    integer ( kind = 4 ) N, the number of points.
!
!    real ( kind = 8 ) TABLE(M,N), the table data.
!
!  Output:
!
!    real ( kind = 8 ) TABLE2(M-2,N-2), the new table data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) table(m,n)
  real ( kind = 8 ) table2(m-2,n-2)

  if ( m <= 2 .or. n <= 2 ) then
    return
  end if

  table2(1:m-2,1:n-2) = table(2:m-1,2:n-1)

  return
end
subroutine r8mat_cholesky_factor ( n, a, c, flag )

!*****************************************************************************80
!
!! r8mat_cholesky_factor computes the Cholesky factor of a symmetric matrix.
!
!  Discussion:
!
!    The matrix must be symmetric and positive semidefinite.
!
!    For a positive semidefinite symmetric matrix A, the Cholesky factorization
!    is a lower triangular matrix L such that:
!
!      A = L * L'
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns.
!
!    real ( kind = 8 ) A(N,N), the N by N matrix.
!
!  Output:
!
!    real ( kind = 8 ) C(N,N), the lower triangular Cholesky factor.
!
!    integer ( kind = 4 ) FLAG:
!    0, no error occurred.
!    1, the matrix is not positive definite.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) c(n,n)
  integer ( kind = 4 ) flag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) sum2

  flag = 0

  c(1:n,1:n) = a(1:n,1:n)

  do j = 1, n

    c(1:j-1,j) = 0.0D+00

    do i = j, n

      sum2 = c(j,i) - dot_product ( c(j,1:j-1), c(i,1:j-1) )

      if ( i == j ) then
        if ( sum2 <= 0.0D+00 ) then
          flag = 1
          return
        else
          c(i,j) = sqrt ( sum2 )
        end if
      else
        if ( c(j,j) /= 0.0D+00 ) then
          c(i,j) = sum2 / c(j,j)
        else
          c(i,j) = 0.0D+00
        end if
      end if

    end do

  end do

  return
end
subroutine r8mat_cholesky_factor_upper ( n, a, c, flag )

!*****************************************************************************80
!
!! R8MAT_CHOLESKY_FACTOR_UPPER: upper Cholesky factor of a symmetric matrix.
!
!  Discussion:
!
!    The matrix must be symmetric and positive semidefinite.
!
!    For a positive semidefinite symmetric matrix A, the Cholesky factorization
!    is an upper triangular matrix R such that:
!
!      A = R * R'
!
!    The lower Cholesky factor is a lower triangular matrix L such that
!
!      A = L * L'
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix A.
!
!    real ( kind = 8 ) A(N,N), the N by N matrix.
!
!  Output:
!
!    real ( kind = 8 ) C(N,N), the N by N upper triangular
!    Cholesky factor.
!
!    integer ( kind = 4 ) FLAG:
!    0, no error occurred.
!    1, the matrix is not positive definite.
!    2, the matrix is not nonnegative definite.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) c(n,n)
  integer ( kind = 4 ) flag
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) sum2

  flag = 0

  c(1:n,1:n) = a(1:n,1:n)

  do j = 1, n

    c(j,1:j-1) = 0.0D+00

    do i = j, n

      sum2 = c(i,j) - dot_product ( c(1:j-1,j), c(1:j-1,i) )

      if ( i == j ) then
        if ( sum2 <= 0.0D+00 ) then
          flag = 1
          return
        else
          c(j,i) = sqrt ( sum2 )
        end if
      else
        if ( c(j,j) /= 0.0D+00 ) then
          c(j,i) = sum2 / c(j,j)
        else
          c(j,i) = 0.0D+00
        end if
      end if

    end do

  end do

  return
end
subroutine r8mat_cholesky_inverse ( n, a )

!*****************************************************************************80
!
!! R8MAT_CHOLESKY_INVERSE computes the inverse of a symmetric matrix.
!
!  Discussion:
!
!    The matrix must be symmetric and positive semidefinite.
!
!    The upper triangular Cholesky factorization R is computed, so that:
!
!      A = R' * R
!
!    Then the inverse B is computed by
!
!      B = inv ( A ) = inv ( R ) * inv ( R' )
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix A.
!
!    real ( kind = 8 ) A(N,N), the matrix to be inverted.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), the inverse of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) s
  real ( kind = 8 ) t

  do j = 1, n

    s = 0.0D+00

    do k = 1, j - 1
      t = a(k,j) - dot_product ( a(1:k-1,k), a(1:k-1,j) )
      t = t / a(k,k)
      a(k,j) = t
      s = s + t * t
    end do

    s = a(j,j) - s

    if ( s <= 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_CHOLESKY_INVERSE - Fatal error!'
      write ( *, '(a)' ) '  The matrix is singular.'
      stop 1
    end if

    a(j,j) = sqrt ( s )

    a(j+1:n,j) = 0.0D+00

  end do
!
!  Compute inverse(R).
!
  do k = 1, n

    a(k,k) = 1.0D+00 / a(k,k)
    a(1:k-1,k) = - a(1:k-1,k) * a(k,k)

    do j = k + 1, n
      t = a(k,j)
      a(k,j) = 0.0D+00
      a(1:k,j) = a(1:k,j) + t * a(1:k,k)
    end do

  end do
!
!  Form inverse(R) * (inverse(R))'.
!
  do j = 1, n
    do k = 1, j - 1
      t = a(k,j)
      a(1:k,k) = a(1:k,k) + t * a(1:k,j)
    end do
    t = a(j,j)
    a(1:j,j) = a(1:j,j) * t
  end do
!
!  Use reflection.
!
  do i = 1, n
    do j = 1, i - 1
      a(i,j) = a(j,i)
    end do
  end do

  return
end
subroutine r8mat_cholesky_solve ( n, l, b, x )

!*****************************************************************************80
!
!! r8mat_cholesky_solve solves a Cholesky factored linear system A * x = b.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    This routine works with the lower triangular Cholesky factor A = L * L'.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns.
!
!    real ( kind = 8 ) L(N,N), the N by N lower Cholesky factor of the
!    system matrix A.
!
!    real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) b(n)
  real ( kind = 8 ) l(n,n)
  real ( kind = 8 ) x(n)
!
!  Solve L * y = b.
!
  call r8mat_l_solve ( n, l, b, x )
!
!  Solve L' * x = y.
!
  call r8mat_lt_solve ( n, l, x, x )

  return
end
subroutine r8mat_cholesky_solve_upper ( n, r, b, x )

!*****************************************************************************80
!
!! R8MAT_CHOLESKY_SOLVE_UPPER solves a Cholesky factored system A * x = b.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    This routine works with the upper triangular Cholesky factor A = R' * R.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix A.
!
!    real ( kind = 8 ) R(N,N), the N by N upper Cholesky factor of the
!    system matrix.
!
!    real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) b(n)
  real ( kind = 8 ) r(n,n)
  real ( kind = 8 ) x(n)
!
!  Solve R' * y = b.
!
  call r8mat_ut_solve ( n, r, b, x )
!
!  Solve R * x = y.
!
  call r8mat_u_solve ( n, r, x, x )

  return
end
subroutine r8mat_copy ( m, n, a, b )

!*****************************************************************************80
!
!! R8MAT_COPY copies an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 2008
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the order of the matrix.
!
!    real ( kind = 8 ) A(M,N), the matrix to be copied.
!
!  Output:
!
!    real ( kind = 8 ) B(M,N), a copy of the matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) b(m,n)

  b(1:m,1:n) = a(1:m,1:n)

  return
end
subroutine r8mat_covariance ( m, n, x, c )

!*****************************************************************************80
!
!! R8MAT_COVARIANCE computes the sample covariance of a set of vector data.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Input:
!
!    integer ( kind = 4 ) M, the size of a single data vectors.
!
!    integer ( kind = 4 ) N, the number of data vectors.
!    N should be greater than 1.
!
!    real ( kind = 8 ) X(M,N), an array of N data vectors, each
!    of length M.
!
!  Output:
!
!    real ( kind = 8 ) C(M,M), the covariance matrix for the data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) c(m,m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(m,n)
  real ( kind = 8 ) x_mean(m)

  c(1:m,1:m) = 0.0D+00
!
!  Special case of N = 1.
!
  if ( n == 1 ) then
    do i = 1, m
      c(i,i) = 1.0D+00
    end do
    return
  end if
!
!  Determine the sample means.
!
  do i = 1, m
    x_mean(i) = sum ( x(i,1:n) ) / real ( n, kind = 8 )
  end do
!
!  Determine the sample covariance.
!
  do j = 1, m
    do i = 1, m
      do k = 1, n
        c(i,j) = c(i,j) + ( x(i,k) - x_mean(i) ) * ( x(j,k) - x_mean(j) )
      end do
    end do
  end do

  c(1:m,1:m) = c(1:m,1:m) / real ( n - 1, kind = 8 )

  return
end
subroutine r8mat_det ( n, a, det )

!*****************************************************************************80
!
!! R8MAT_DET computes the determinant of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    Original FORTRAN77 version by Helmut Spaeth.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Helmut Spaeth,
!    Cluster Analysis Algorithms
!    for Data Reduction and Classification of Objects,
!    Ellis Horwood, 1980, page 125-127.
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix.
!
!    real ( kind = 8 ) A(N,N), the matrix whose determinant is desired.
!
!  Output:
!
!    real ( kind = 8 ) DET, the determinant of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) piv(1)
  real ( kind = 8 ) t

  b(1:n,1:n) = a(1:n,1:n)

  det = 1.0D+00

  do k = 1, n

    piv = maxloc ( abs ( b(k:n,k) ) )

    m = piv(1) + k - 1

    if ( m /= k ) then
      det = - det
      t      = b(m,k)
      b(m,k) = b(k,k)
      b(k,k) = t
    end if

    det = det * b(k,k)

    if ( b(k,k) /= 0.0D+00 ) then

      b(k+1:n,k) = -b(k+1:n,k) / b(k,k)

      do j = k + 1, n
        if ( m /= k ) then
          t      = b(m,j)
          b(m,j) = b(k,j)
          b(k,j) = t
        end if
        b(k+1:n,j) = b(k+1:n,j) + b(k+1:n,k) * b(k,j)
      end do

    end if

  end do

  return
end
function r8mat_det_2d ( a )

!*****************************************************************************80
!
!! R8MAT_DET_2D computes the determinant of a 2 by 2 R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The formula for the determinant of a 2 by 2 matrix is
!
!      a11 * a22 - a12 * a21.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A(2,2), the matrix whose determinant is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_DET_2D, the determinant of the matrix.
!
  implicit none

  real ( kind = 8 ) a(2,2)
  real ( kind = 8 ) r8mat_det_2d

  r8mat_det_2d = a(1,1) * a(2,2) - a(1,2) * a(2,1)

  return
end
function r8mat_det_3d ( a )

!*****************************************************************************80
!
!! R8MAT_DET_3D computes the determinant of a 3 by 3 R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The formula for the determinant of a 3 by 3 matrix is
!
!        a11 * a22 * a33 - a11 * a23 * a32
!      + a12 * a23 * a31 - a12 * a21 * a33
!      + a13 * a21 * a32 - a13 * a22 * a31
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A(3,3), the matrix whose determinant is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_DET_3D, the determinant of the matrix.
!
  implicit none

  real ( kind = 8 ) a(3,3)
  real ( kind = 8 ) r8mat_det_3d

  r8mat_det_3d = &
         a(1,1) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) &
       + a(1,2) * ( a(2,3) * a(3,1) - a(2,1) * a(3,3) ) &
       + a(1,3) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) )

  return
end
function r8mat_det_4d ( a )

!*****************************************************************************80
!
!! R8MAT_DET_4D computes the determinant of a 4 by 4 R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A(4,4), the matrix whose determinant is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_DET_4D, the determinant of the matrix.
!
  implicit none

  real ( kind = 8 ) a(4,4)
  real ( kind = 8 ) r8mat_det_4d

  r8mat_det_4d = &
         a(1,1) * ( &
             a(2,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
           - a(2,3) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) &
           + a(2,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) ) &
       - a(1,2) * ( &
             a(2,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
           - a(2,3) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) ) &
           + a(2,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) ) &
       + a(1,3) * ( &
             a(2,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) &
           - a(2,2) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) ) &
           + a(2,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) ) &
       - a(1,4) * ( &
             a(2,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
           - a(2,2) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) &
           + a(2,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) )

  return
end
function r8mat_det_5d ( a )

!*****************************************************************************80
!
!! R8MAT_DET_5D computes the determinant of a 5 by 5 R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A(5,5), the matrix whose determinant is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_DET_5D, the determinant of the matrix.
!
  implicit none

  real ( kind = 8 ) a(5,5)
  real ( kind = 8 ) b(4,4)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8mat_det_4d
  real ( kind = 8 ) r8mat_det_5d
!
!  Expand the determinant into the sum of the determinants of the
!  five 4 by 4 matrices created by dropping row 1, and column k.
!
  r8mat_det_5d = 0.0D+00

  do k = 1, 5

    do i = 1, 4
      do j = 1, 4

        if ( j < k ) then
          inc = 0
        else
          inc = 1
        end if

        b(i,j) = a(i+1,j+inc)

      end do
    end do

    r8mat_det_5d = r8mat_det_5d &
      + ( -1 ) ** ( k + 1 ) * a(1,k) * r8mat_det_4d ( b )

  end do

  return
end
subroutine r8mat_diag_add_scalar ( n, a, s )

!*****************************************************************************80
!
!! R8MAT_DIAG_ADD_SCALAR adds a scalar to the diagonal of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns.
!
!    real ( kind = 8 ) A(N,N), the N by N matrix to be modified.
!
!    real ( kind = 8 ) S, the value to be added to the diagonal
!    of the matrix.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), the modified matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) s

  do i = 1, n
    a(i,i) = a(i,i) + s
  end do

  return
end
subroutine r8mat_diag_add_vector ( n, a, v )

!*****************************************************************************80
!
!! R8MAT_DIAG_ADD_VECTOR adds a vector to the diagonal of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix.
!
!    real ( kind = 8 ) A(N,N), the N by N matrix.
!
!    real ( kind = 8 ) V(N), the vector to be added to the diagonal of A.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), the modified matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)

  do i = 1, n
    a(i,i) = a(i,i) + v(i)
  end do

  return
end
subroutine r8mat_diag_get_vector ( n, a, v )

!*****************************************************************************80
!
!! R8MAT_DIAG_GET_VECTOR gets the value of the diagonal of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix.
!
!    real ( kind = 8 ) A(N,N), the N by N matrix.
!
!  Output:
!
!    real ( kind = 8 ) V(N), the diagonal entries
!    of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)

  do i = 1, n
    v(i) = a(i,i)
  end do

  return
end
subroutine r8mat_diag_set_scalar ( n, a, s )

!*****************************************************************************80
!
!! R8MAT_DIAG_SET_SCALAR sets the diagonal of an R8MAT to a scalar value.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns.
!
!    real ( kind = 8 ) A(N,N), the N by N matrix to be modified.
!
!    real ( kind = 8 ) S, the value to be assigned to the diagonal
!    of the matrix.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), the modified matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) s

  do i = 1, n
    a(i,i) = s
  end do

  return
end
subroutine r8mat_diag_set_vector ( n, a, v )

!*****************************************************************************80
!
!! R8MAT_DIAG_SET_VECTOR sets the diagonal of an R8MAT to a vector.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns.
!
!    real ( kind = 8 ) A(N,N), the N by N matrix.
!
!    real ( kind = 8 ) V(N), the vector to be assigned to the
!    diagonal of A.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), the modified matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)

  do i = 1, n
    a(i,i) = v(i)
  end do

  return
end
subroutine r8mat_diagonal ( n, diag, a )

!*****************************************************************************80
!
!! R8MAT_DIAGONAL returns a diagonal matrix as an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of A.
!
!    real ( kind = 8 ) DIAG(N), the diagonal entries.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), the N by N diagonal matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) diag(n)
  integer ( kind = 4 ) i

  a(1:n,1:n) = 0.0D+00

  do i = 1, n
    a(i,i) = diag(i)
  end do

  return
end
function r8mat_diff_frobenius ( m, n, a1, a2 )

!*****************************************************************************80
!
!! R8MAT_DIFF_FROBENIUS returns the Frobenius norm of an R8MAT difference.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Frobenius norm is defined as
!
!      R8MAT_DIFF_FROBENIUS = sqrt (
!        sum ( 1 <= I <= M ) sum ( 1 <= j <= N ) A(I,J) * A(I,J) )
!
!    The matrix Frobenius norm is not derived from a vector norm, but
!    is compatible with the vector L2 norm, so that:
!
!      r8vec_norm_l2 ( A * x ) <= 
!        r8mat_diff_frobenius ( A ) * r8vec_norm_l2 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows.
!
!    integer ( kind = 4 ) N, the number of columns.
!
!    real ( kind = 8 ) A1(M,N), A2(M,N), the matrices for whose 
!    difference the Frobenius norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_DIFF_FROBENIUSE, the Frobenius 
!    norm of A1 - A2.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(m,n)
  real ( kind = 8 ) a2(m,n)
  real ( kind = 8 ) r8mat_diff_frobenius

  r8mat_diff_frobenius = sqrt ( sum ( ( a1(1:m,1:n) - a2(1:m,1:n) ) ** 2 ) )

  return
end
subroutine r8mat_expand_linear ( m, n, x, mfat, nfat, xfat )

!*****************************************************************************80
!
!! R8MAT_EXPAND_LINEAR linearly interpolates new data into an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In this routine, the expansion is specified by giving the number
!    of intermediate values to generate between each pair of original
!    data rows and columns.
!
!    The interpolation is not actually linear.  It uses the functions
!
!      1, x, y, and xy.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns of
!    input data.
!
!    real ( kind = 8 ) X(M,N), the original data.
!
!    integer ( kind = 4 ) MFAT, NFAT, the number of data values
!    to interpolate between each row, and each column, of original data values.
!
!  Output:
!
!    real ( kind = 8 ) XFAT(M2,N2), the fattened data, where
!    M2 = (M-1)*(MFAT+1)+1,
!    N2 = (N-1)*(NFAT+1)+1.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) mfat
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nfat

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) iii
  integer ( kind = 4 ) ip1
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jj
  integer ( kind = 4 ) jjj
  integer ( kind = 4 ) jp1
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) x(m,n)
  real ( kind = 8 ) x00
  real ( kind = 8 ) x01
  real ( kind = 8 ) x10
  real ( kind = 8 ) x11
  real ( kind = 8 ) xfat((m-1)*(mfat+1)+1,(n-1)*(nfat+1)+1)

  do i = 1, m

    if ( i < m ) then
      ihi = mfat
    else
      ihi = 0
    end if

    do j = 1, n

      if ( j < n ) then
        jhi = nfat
      else
        jhi = 0
      end if

      if ( i < m ) then
        ip1 = i + 1
      else
        ip1 = i
      end if

      if ( j < n ) then
        jp1 = j + 1
      else
        jp1 = j
      end if

      x00 = x(i,j)
      x10 = x(ip1,j)
      x01 = x(i,jp1)
      x11 = x(ip1,jp1)

      do ii = 0, ihi

        s = real ( ii, kind = 8 ) &
          / real ( ihi + 1, kind = 8 )

        do jj = 0, jhi

          t = real ( jj, kind = 8 ) &
            / real ( jhi + 1, kind = 8 )

          iii = 1 + ( i - 1 ) * ( mfat + 1 ) + ii
          jjj = 1 + ( j - 1 ) * ( nfat + 1 ) + jj

          xfat(iii,jjj) = &
                                            x00   &
              + s     * (       x10       - x00 ) &
              + t     * (             x01 - x00 ) &
              + s * t * ( x11 - x10 - x01 + x00 )

        end do

      end do

    end do

  end do

  return
end
subroutine r8mat_expand_linear2 ( m, n, a, m2, n2, a2 )

!*****************************************************************************80
!
!! R8MAT_EXPAND_LINEAR2 expands an R8MAT by linear interpolation.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In this version of the routine, the expansion is indicated
!    by specifying the dimensions of the expanded array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns in A.
!
!    real ( kind = 8 ) A(M,N), a "small" M by N array.
!
!    integer ( kind = 4 ) M2, N2, the number of rows and columns in A2.
!
!  Output:
!
!    real ( kind = 8 ) A2(M2,N2), the expanded array, which
!    contains an interpolated version of the data in A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) a2(m2,n2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  real ( kind = 8 ) r
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  real ( kind = 8 ) s
  real ( kind = 8 ) s1
  real ( kind = 8 ) s2

  do i = 1, m2

    if ( m2 == 1 ) then
      r = 0.5D+00
    else
      r = real ( i - 1, kind = 8 ) &
        / real ( m2 - 1, kind = 8 )
    end if

    i1 = 1 + int ( r * real ( m - 1, kind = 8 ) )
    i2 = i1 + 1

    if ( m < i2 ) then
      i1 = m - 1
      i2 = m
    end if

    r1 = real ( i1 - 1, kind = 8 ) &
       / real ( m - 1, kind = 8 )

    r2 = real ( i2 - 1, kind = 8 ) &
       / real ( m - 1, kind = 8 )

    do j = 1, n2

      if ( n2 == 1 ) then
        s = 0.5D+00
      else
        s = real ( j - 1, kind = 8 ) &
          / real ( n2 - 1, kind = 8 )
      end if

      j1 = 1 + int ( s * real ( n - 1, kind = 8 ) )
      j2 = j1 + 1

      if ( n < j2 ) then
        j1 = n - 1
        j2 = n
      end if

      s1 = real ( j1 - 1, kind = 8 ) &
         / real ( n - 1, kind = 8 )

      s2 = real ( j2 - 1, kind = 8 ) &
         / real ( n - 1, kind = 8 )

      a2(i,j) = &
        ( ( r2 - r ) * ( s2 - s ) * a(i1,j1) &
        + ( r - r1 ) * ( s2 - s ) * a(i2,j1) &
        + ( r2 - r ) * ( s - s1 ) * a(i1,j2) &
        + ( r - r1 ) * ( s - s1 ) * a(i2,j2) ) &
        / ( ( r2 - r1 ) * ( s2 - s1 ) )

    end do

  end do

  return
end
subroutine r8mat_flip_cols ( m, n, a, b )

!*****************************************************************************80
!
!! R8MAT_FLIP_COLS reverses the column order of an R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the order of the matrix.
!
!    real ( kind = 8 ) A(M,N), the matrix to be flipped.
!
!  Output:
!
!    real ( kind = 8 ) B(M,N), a copy of A, with the columns
!    in reverse order.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) b(m,n)

  b(1:m,n:1:-1) = a(1:m,1:n)

  return
end
subroutine r8mat_flip_rows ( m, n, a, b )

!*****************************************************************************80
!
!! R8MAT_FLIP_ROWS reverses the row order of an R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the order of the matrix.
!
!    real ( kind = 8 ) A(M,N), the matrix to be flipped.
!
!  Output:
!
!    real ( kind = 8 ) B(M,N), a copy of A, with the rows
!    in reverse order.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) b(m,n)

  b(m:1:-1,1:n) = a(1:m,1:n)

  return
end
subroutine r8mat_fs ( n, a, b, info )

!*****************************************************************************80
!
!! r8mat_fs() factors and solves a system with one right hand side.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    This routine differs from R8MAT_FSS in two ways:
!    * only one right hand side is allowed;
!    * the input matrix A is not modified.
!
!    This routine uses partial pivoting, but no pivot vector is required.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    21 January 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    real ( kind = 8 ) A(N,N), the coefficient matrix of the linear system.
!
!    real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), a unit upper triangular matrix,
!    the U factor of an LU factorization of the
!    original coefficient matrix.
!
!    real ( kind = 8 ) B(N), the solution of the linear systems.
!
!    integer ( kind = 4 ) INFO, singularity flag.
!    0, no singularity detected.
!    nonzero, the factorization failed on the INFO-th step.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) a2(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipiv
  integer ( kind = 4 ) jcol
  real ( kind = 8 ) piv
  real ( kind = 8 ) row(n)
  real ( kind = 8 ) t
  real ( kind = 8 ) temp

  a2(1:n,1:n) = a(1:n,1:n)

  info = 0

  do jcol = 1, n
!
!  Find the maximum element in column I.
!
    piv = abs ( a2(jcol,jcol) )
    ipiv = jcol
    do i = jcol + 1, n
      if ( piv < abs ( a2(i,jcol) ) ) then
        piv = abs ( a2(i,jcol) )
        ipiv = i
      end if
    end do

    if ( piv == 0.0D+00 ) then
      info = jcol
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_FS - Fatal error!'
      write ( *, '(a,i8)' ) '  Zero pivot on step ', info
      stop 1
    end if
!
!  Switch rows JCOL and IPIV, and B.
!
    if ( jcol /= ipiv ) then

      row(1:n) = a2(jcol,1:n)
      a2(jcol,1:n) = a2(ipiv,1:n)
      a2(ipiv,1:n) = row(1:n)

      t       = b(jcol)
      b(jcol) = b(ipiv)
      b(ipiv) = t

    end if
!
!  Scale the pivot row.
!
    a2(jcol,jcol+1:n) = a2(jcol,jcol+1:n) / a2(jcol,jcol)
    b(jcol) = b(jcol) / a2(jcol,jcol)
    a2(jcol,jcol) = 1.0D+00
!
!  Use the pivot row to eliminate lower entries in that column.
!
    do i = jcol + 1, n
      if ( a2(i,jcol) /= 0.0D+00 ) then
        temp = - a2(i,jcol)
        a2(i,jcol) = 0.0D+00
        a2(i,jcol+1:n) = a2(i,jcol+1:n) + temp * a2(jcol,jcol+1:n)
        b(i) = b(i) + temp * b(jcol)
      end if
    end do

  end do
!
!  Back solve.
!
  do jcol = n, 2, -1
    b(1:jcol-1) = b(1:jcol-1) - a2(1:jcol-1,jcol) * b(jcol)
  end do

  return
end
subroutine r8mat_fss ( n, a, nb, b, info )

!*****************************************************************************80
!
!! R8MAT_FSS factors and solves a system with multiple right hand sides.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    This routine does not save the LU factors of the matrix, and hence cannot
!    be used to efficiently solve multiple linear systems, or even to
!    factor A at one time, and solve a single linear system at a later time.
!
!    This routine uses partial pivoting, but no pivot vector is required.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    real ( kind = 8 ) A(N,N), the coefficient matrix of the linear system.
!
!    integer ( kind = 4 ) NB, the number of right hand sides.
!
!    real ( kind = 8 ) B(N,NB), the right hand sides of the linear system.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), A is in unit upper triangular form, and
!    represents the U factor of an LU factorization of the
!    original coefficient matrix.
!
!    real ( kind = 8 ) B(N,NB), the solutions of the linear systems.
!
!    integer ( kind = 4 ) INFO, singularity flag.
!    0, no singularity detected.
!    nonzero, the factorization failed on the INFO-th step.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) nb

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,nb)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipiv
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jcol
  real ( kind = 8 ) piv
  real ( kind = 8 ) row(n)
  real ( kind = 8 ) t(nb)
  real ( kind = 8 ) temp

  info = 0

  do jcol = 1, n
!
!  Find the maximum element in column I.
!
    piv = abs ( a(jcol,jcol) )
    ipiv = jcol
    do i = jcol + 1, n
      if ( piv < abs ( a(i,jcol) ) ) then
        piv = abs ( a(i,jcol) )
        ipiv = i
      end if
    end do

    if ( piv == 0.0D+00 ) then
      info = jcol
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_FSS - Fatal error!'
      write ( *, '(a,i8)' ) '  Zero pivot on step ', info
      stop 1
    end if
!
!  Switch rows JCOL and IPIV, and B.
!
    if ( jcol /= ipiv ) then

      row(1:n) = a(jcol,1:n)
      a(jcol,1:n) = a(ipiv,1:n)
      a(ipiv,1:n) = row(1:n)

      t(1:nb)      = b(jcol,1:nb)
      b(jcol,1:nb) = b(ipiv,1:nb)
      b(ipiv,1:nb) = t(1:nb)

    end if
!
!  Scale the pivot row.
!
    a(jcol,jcol+1:n) = a(jcol,jcol+1:n) / a(jcol,jcol)
    b(jcol,1:nb) = b(jcol,1:nb) / a(jcol,jcol)
    a(jcol,jcol) = 1.0D+00
!
!  Use the pivot row to eliminate lower entries in that column.
!
    do i = jcol + 1, n
      if ( a(i,jcol) /= 0.0D+00 ) then
        temp = - a(i,jcol)
        a(i,jcol) = 0.0D+00
        a(i,jcol+1:n) = a(i,jcol+1:n) + temp * a(jcol,jcol+1:n)
        b(i,1:nb) = b(i,1:nb) + temp * b(jcol,1:nb)
      end if
    end do

  end do
!
!  Back solve.
!
  do j = 1, nb
    do jcol = n, 2, -1
      b(1:jcol-1,j) = b(1:jcol-1,j) - a(1:jcol-1,jcol) * b(jcol,j)
    end do
  end do

  return
end
subroutine r8mat_givens_post ( n, a, row, col, g )

!*****************************************************************************80
!
!! R8MAT_GIVENS_POST computes the Givens postmultiplier rotation matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Givens post-multiplier matrix G(ROW,COL) has the property that
!    the (ROW,COL)-th entry of A*G is zero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrices A and G.
!
!    real ( kind = 8 ) A(N,N), the matrix to be operated upon.
!
!    integer ( kind = 4 ) ROW, COL, the row and column of the
!    entry of A*G which is to be zeroed out.
!
!  Output:
!
!    real ( kind = 8 ) G(N,N), the Givens rotation matrix.
!    G is an orthogonal matrix, that is, the inverse of
!    G is the transpose of G.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) col
  real ( kind = 8 ) g(n,n)
  integer ( kind = 4 ) row
  real ( kind = 8 ) theta

  call r8mat_identity ( n, g )

  theta = atan2 ( a(row,col), a(row,row) )

  g(row,row) =  cos ( theta )
  g(row,col) = -sin ( theta )
  g(col,row) =  sin ( theta )
  g(col,col) =  cos ( theta )

  return
end
subroutine r8mat_givens_pre ( n, a, row, col, g )

!*****************************************************************************80
!
!! R8MAT_GIVENS_PRE computes the Givens premultiplier rotation matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Givens premultiplier rotation matrix G(ROW,COL) has the
!    property that the (ROW,COL)-th entry of G*A is zero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrices A and G.
!
!    real ( kind = 8 ) A(N,N), the matrix to be operated upon.
!
!    integer ( kind = 4 ) ROW, COL, the row and column of the
!    entry of the G*A which is to be zeroed out.
!
!  Output:
!
!    real ( kind = 8 ) G(N,N), the Givens rotation matrix.
!    G is an orthogonal matrix, that is, the inverse of
!    G is the transpose of G.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) col
  real ( kind = 8 ) g(n,n)
  integer ( kind = 4 ) row
  real ( kind = 8 ) theta

  call r8mat_identity ( n, g )

  theta = atan2 ( a(row,col), a(col,col) )

  g(row,row) =  cos ( theta )
  g(row,col) = -sin ( theta )
  g(col,row) =  sin ( theta )
  g(col,col) =  cos ( theta )

  return
end
subroutine r8mat_hess ( fx, n, x, h )

!*****************************************************************************80
!
!! R8MAT_HESS approximates a Hessian matrix via finite differences.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    H(I,J) = d2 F / d X(I) d X(J)
!
!    The values returned by this routine will be only approximate.
!    In some cases, they will be so poor that they are useless.
!    However, one of the best applications of this routine is for
!    checking your own Hessian calculations, since as Heraclitus
!    said, you'll never get the same result twice when you differentiate
!    a complicated expression by hand.
!
!    The user function routine, here called "FX", should have the form:
!
!      subroutine fx ( n, x, f )
!      integer ( kind = 4 ) n
!      real ( kind = 8 ) f
!      real ( kind = 8 ) x(n)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    external FX, the name of the user function routine.
!
!    integer ( kind = 4 ) N, the number of variables.
!
!    real ( kind = 8 ) X(N), the values of the variables.
!
!  Output:
!
!    real ( kind = 8 ) H(N,N), the approximated N by N Hessian matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) eps
  real ( kind = 8 ) f00
  real ( kind = 8 ) fmm
  real ( kind = 8 ) fmp
  real ( kind = 8 ) fpm
  real ( kind = 8 ) fpp
  external fx
  real ( kind = 8 ) h(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) s(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xi
  real ( kind = 8 ) xj
!
!  Choose the stepsizes.
!
  eps = ( epsilon ( eps ) ) ** 0.33D+00

  do i = 1, n
    s(i) = eps * max ( abs ( x(i) ), 1.0D+00 )
  end do
!
!  Calculate the diagonal elements.
!
  do i = 1, n

    xi = x(i)

    call fx ( n, x, f00 )

    x(i) = xi + s(i)
    call fx ( n, x, fpp )

    x(i) = xi - s(i)
    call fx ( n, x, fmm )

    h(i,i) = ( ( fpp - f00 ) + ( fmm - f00 ) ) / s(i) ** 2

    x(i) = xi

  end do
!
!  Calculate the off diagonal elements.
!
  do i = 1, n

    xi = x(i)

    do j = i + 1, n

      xj = x(j)

      x(i) = xi + s(i)
      x(j) = xj + s(j)
      call fx ( n, x, fpp )

      x(i) = xi + s(i)
      x(j) = xj - s(j)
      call fx ( n, x, fpm )

      x(i) = xi - s(i)
      x(j) = xj + s(j)
      call fx ( n, x, fmp )

      x(i) = xi - s(i)
      x(j) = xj - s(j)
      call fx ( n, x, fmm )

      h(j,i) = ( ( fpp - fpm ) + ( fmm - fmp ) ) / ( 4.0D+00 * s(i) * s(j) )

      h(i,j) = h(j,i)

      x(j) = xj

    end do

    x(i) = xi

  end do

  return
end
subroutine r8mat_house_axh ( n, a, v, ah )

!*****************************************************************************80
!
!! R8MAT_HOUSE_AXH computes A*H where H is a compact Householder matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Householder matrix H(V) is defined by
!
!      H(V) = I - 2 * v * v' / ( v' * v )
!
!    This routine is not particularly efficient.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of A.
!
!    real ( kind = 8 ) A(N,N), the matrix to be postmultiplied.
!
!    real ( kind = 8 ) V(N), a vector defining a Householder matrix.
!
!  Output:
!
!    real ( kind = 8 ) AH(N,N), the product A*H.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) ah(n,n)
  real ( kind = 8 ) ah_temp(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) v_normsq

  v_normsq = sum ( v(1:n) ** 2 )
!
!  Compute A*H' = A*H
!
  do i = 1, n
    do j = 1, n
      ah_temp(i,j) = a(i,j)
      do k = 1, n
        ah_temp(i,j) = ah_temp(i,j) - 2.0D+00 * a(i,k) * v(k) * v(j) / v_normsq
      end do
    end do
  end do
!
!  Copy the temporary result into AH.
!  Doing it this way means the user can identify the input arguments A and AH.
!
  ah(1:n,1:n) = ah_temp(1:n,1:n)

  return
end
subroutine r8mat_house_form ( n, v, h )

!*****************************************************************************80
!
!! R8MAT_HOUSE_FORM constructs a Householder matrix from its compact form.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    H(v) = I - 2 * v * v' / ( v' * v )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix.
!
!    real ( kind = 8 ) V(N), the vector defining the Householder matrix.
!
!  Output:
!
!    real ( kind = 8 ) H(N,N), the Householder matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) beta
  real ( kind = 8 ) h(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) v(n)
!
!  Compute the L2 norm of V.
!
  beta = sum ( v(1:n) ** 2 )
!
!  Form the matrix H.
!
  call r8mat_identity ( n, h )

  do i = 1, n
    do j = 1, n
      h(i,j) = h(i,j) - 2.0D+00 * v(i) * v(j) / beta
    end do
  end do

  return
end
subroutine r8mat_house_hxa ( n, a, v, ha )

!*****************************************************************************80
!
!! R8MAT_HOUSE_HXA computes H*A where H is a compact Householder matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Householder matrix H(V) is defined by
!
!      H(V) = I - 2 * v * v' / ( v' * v )
!
!    This routine is not particularly efficient.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of A.
!
!    real ( kind = 8 ) A(N,N), the matrix to be premultiplied.
!
!    real ( kind = 8 ) V(N), a vector defining a Householder matrix.
!
!  Output:
!
!    real ( kind = 8 ) HA(N,N), the product H*A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) ha(n,n)
  real ( kind = 8 ) ha_temp(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) v_normsq

  v_normsq = sum ( v(1:n) ** 2 )
!
!  Compute A*H' = A*H
!
  do i = 1, n
    do j = 1, n
      ha_temp(i,j) = a(i,j)
      do k = 1, n
        ha_temp(i,j) = ha_temp(i,j) - 2.0D+00 * v(i) * v(k) * a(k,j) / v_normsq
      end do
    end do
  end do
!
!  Copy the temporary result into HA.
!  Doing it this way means the user can identify the input arguments A and HA.
!
  ha(1:n,1:n) = ha_temp(1:n,1:n)

  return
end
subroutine r8mat_house_post ( n, a, row, col, h )

!*****************************************************************************80
!
!! R8MAT_HOUSE_POST computes a Householder post-multiplier matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    H(ROW,COL) has the property that the ROW-th column of
!    A*H(ROW,COL) is zero from entry COL+1 to the end.
!
!    In the most common case, where a QR factorization is being computed,
!    ROW = COL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrices.
!
!    real ( kind = 8 ) A(N,N), the matrix whose Householder matrix
!    is to be computed.
!
!    integer ( kind = 4 ) ROW, COL, specify the location of the
!    entry of the matrix A which is to be preserved.  The entries in
!    the same row, but higher column, will be zeroed out if
!    A is postmultiplied by H.
!
!  Output:
!
!    real ( kind = 8 ) H(N,N), the Householder matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) a_row(n)
  integer ( kind = 4 ) col
  real ( kind = 8 ) h(n,n)
  integer ( kind = 4 ) row
  real ( kind = 8 ) v(n)
!
!  Set up the vector V.
!
  a_row(1:col-1) = 0.0D+00
  a_row(col:n) = a(row,col:n)

  call r8vec_house_column ( n, a_row, col, v )
!
!  Form the matrix H(V).
!
  call r8mat_house_form ( n, v, h )

  return
end
subroutine r8mat_house_pre ( n, a, row, col, h )

!*****************************************************************************80
!
!! R8MAT_HOUSE_PRE computes a Householder pre-multiplier matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    H(ROW,COL) has the property that the COL-th column of
!    H(ROW,COL)*A is zero from entry ROW+1 to the end.
!
!    In the most common case, where a QR factorization is being computed,
!    ROW = COL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrices.
!
!    real ( kind = 8 ) A(N,N), the matrix whose Householder matrix
!    is to be computed.
!
!    integer ( kind = 4 ) ROW, COL, specify the location of the
!    entry of the matrix A which is to be preserved.  The entries in
!    the same column, but higher rows, will be zeroed out if A is
!    premultiplied by H.
!
!  Output:
!
!    real ( kind = 8 ) H(N,N), the Householder matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) a_col(n)
  integer ( kind = 4 ) col
  real ( kind = 8 ) h(n,n)
  integer ( kind = 4 ) row
  real ( kind = 8 ) v(n)
!
!  Get the vector V.
!
  a_col(1:row-1) = 0.0D+00
  a_col(row:n) = a(row:n,col)

  call r8vec_house_column ( n, a_col, row, v )
!
!  Form the matrix H(V).
!
  call r8mat_house_form ( n, v, h )

  return
end
subroutine r8mat_identity ( n, a )

!*****************************************************************************80
!
!! R8MAT_IDENTITY stores the identity matrix in an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of A.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), the N by N identity matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i

  a(1:n,1:n) = 0.0D+00

  do i = 1, n
    a(i,i) = 1.0D+00
  end do

  return
end
subroutine r8mat_indicator ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_INDICATOR sets up an "indicator" R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The value of each entry suggests its location, as in:
!
!      11  12  13  14
!      21  22  23  24
!      31  32  33  34
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 May 2008
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows of the matrix.
!    M must be positive.
!
!    integer ( kind = 4 ) N, the number of columns of the matrix.
!    N must be positive.
!
!  Output:
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) fac
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_log_10
  integer ( kind = 4 ) j

  fac = 10 ** ( i4_log_10 ( n ) + 1 )

  do i = 1, m
    do j = 1, n
      a(i,j) = real ( fac * i + j, kind = 8 )
    end do
  end do

  return
end
subroutine r8mat_inverse_2d ( a, b, det )

!*****************************************************************************80
!
!! R8MAT_INVERSE_2D inverts a 2 by 2 R8MAT using Cramer's rule.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If the determinant is zero, then A is singular, and does not have an
!    inverse.  In that case, B is simply set to zero, and a
!    message is printed.
!
!    If the determinant is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A(2,2), the matrix to be inverted.
!
!  Output:
!
!    real ( kind = 8 ) B(2,2), the inverse of the matrix A.
!
!    real ( kind = 8 ) DET, the determinant of the matrix A.
!
  implicit none

  real ( kind = 8 ) a(2,2)
  real ( kind = 8 ) b(2,2)
  real ( kind = 8 ) det
  real ( kind = 8 ) r8mat_det_2d
!
!  Compute the determinant of A.
!
  det = r8mat_det_2d ( a )

  if ( det == 0.0D+00 ) then

    b(1:2,1:2) = 0.0D+00

  else

    b(1,1) =  a(2,2) / det
    b(1,2) = -a(1,2) / det
    b(2,1) = -a(2,1) / det
    b(2,2) =  a(1,1) / det

  end if

  return
end
subroutine r8mat_inverse_3d ( a, b, det )

!*****************************************************************************80
!
!! R8MAT_INVERSE_3D inverts a 3 by 3 R8MAT using Cramer's rule.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If the determinant is zero, then A is singular, and does not have an
!    inverse.  In that case, B is simply set to zero, and a
!    message is printed.
!
!    If the determinant is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A(3,3), the matrix to be inverted.
!
!  Output:
!
!    real ( kind = 8 ) B(3,3), the inverse of the matrix A.
!
!    real ( kind = 8 ) DET, the determinant of the matrix A.
!
  implicit none

  real ( kind = 8 ) a(3,3)
  real ( kind = 8 ) b(3,3)
  real ( kind = 8 ) det
  real ( kind = 8 ) r8mat_det_3d
!
!  Compute the determinant of A.
!
  det = r8mat_det_3d ( a )
!
!  If the determinant is zero, bail out.
!
  if ( det == 0.0D+00 ) then
    b(1:3,1:3) = 0.0D+00
    return
  end if
!
!  Compute the entries of the inverse matrix using an explicit
!  formula.
!
  b(1,1) =  ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) / det
  b(1,2) = -( a(1,2) * a(3,3) - a(1,3) * a(3,2) ) / det
  b(1,3) =  ( a(1,2) * a(2,3) - a(1,3) * a(2,2) ) / det

  b(2,1) = -( a(2,1) * a(3,3) - a(2,3) * a(3,1) ) / det
  b(2,2) =  ( a(1,1) * a(3,3) - a(1,3) * a(3,1) ) / det
  b(2,3) = -( a(1,1) * a(2,3) - a(1,3) * a(2,1) ) / det

  b(3,1) =  ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) / det
  b(3,2) = -( a(1,1) * a(3,2) - a(1,2) * a(3,1) ) / det
  b(3,3) =  ( a(1,1) * a(2,2) - a(1,2) * a(2,1) ) / det

  return
end
subroutine r8mat_inverse_4d ( a, b, det )

!*****************************************************************************80
!
!! R8MAT_INVERSE_4D inverts a 4 by 4 R8MAT using Cramer's rule.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If the determinant is zero, then A is singular, and does not have an
!    inverse.  In that case, B is simply set to zero, and a
!    message is printed.
!
!    If the determinant is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A(4,4), the matrix to be inverted.
!
!  Output:
!
!    real ( kind = 8 ) B(4,4), the inverse of the matrix A.
!
!    real ( kind = 8 ) DET, the determinant of the matrix A.
!
  implicit none

  real ( kind = 8 ) a(4,4)
  real ( kind = 8 ) b(4,4)
  real ( kind = 8 ) det
  real ( kind = 8 ) r8mat_det_4d
!
!  Compute the determinant of A.
!
  det = r8mat_det_4d ( a )
!
!  If the determinant is zero, bail out.
!
  if ( det == 0.0D+00 ) then

    b(1:4,1:4) = 0.0D+00

    return
  end if
!
!  Compute the entries of the inverse matrix using an explicit formula.
!
  b(1,1) = +( &
        + a(2,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
        + a(2,3) * ( a(3,4) * a(4,2) - a(3,2) * a(4,4) ) &
        + a(2,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
        ) / det

  b(2,1) = -( &
        + a(2,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
        + a(2,3) * ( a(3,4) * a(4,1) - a(3,1) * a(4,4) ) &
        + a(2,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) &
        ) / det

  b(3,1) = +( &
        + a(2,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) &
        + a(2,2) * ( a(3,4) * a(4,1) - a(3,1) * a(4,4) ) &
        + a(2,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) &
        ) / det

  b(4,1) = -( &
        + a(2,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
        + a(2,2) * ( a(3,3) * a(4,1) - a(3,1) * a(4,3) ) &
        + a(2,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) &
        ) / det

  b(1,2) = -( &
        + a(1,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
        + a(1,3) * ( a(3,4) * a(4,2) - a(3,2) * a(4,4) ) &
        + a(1,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
        ) / det

  b(2,2) = +( &
        + a(1,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) ) &
        + a(1,3) * ( a(3,4) * a(4,1) - a(3,1) * a(4,4) ) &
        + a(1,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) &
        ) / det

  b(3,2) = -( &
        + a(1,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) ) &
        + a(1,2) * ( a(3,4) * a(4,1) - a(3,1) * a(4,4) ) &
        + a(1,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) &
        ) / det

  b(4,2) = +( &
        + a(1,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) &
        + a(1,2) * ( a(3,3) * a(4,1) - a(3,1) * a(4,3) ) &
        + a(1,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) &
        ) / det

  b(1,3) = +( &
        + a(1,2) * ( a(2,3) * a(4,4) - a(2,4) * a(4,3) ) &
        + a(1,3) * ( a(2,4) * a(4,2) - a(2,2) * a(4,4) ) &
        + a(1,4) * ( a(2,2) * a(4,3) - a(2,3) * a(4,2) ) &
        ) / det

  b(2,3) = -( &
        + a(1,1) * ( a(2,3) * a(4,4) - a(2,4) * a(4,3) ) &
        + a(1,3) * ( a(2,4) * a(4,1) - a(2,1) * a(4,4) ) &
        + a(1,4) * ( a(2,1) * a(4,3) - a(2,3) * a(4,1) ) &
        ) / det

  b(3,3) = +( &
        + a(1,1) * ( a(2,2) * a(4,4) - a(2,4) * a(4,2) ) &
        + a(1,2) * ( a(2,4) * a(4,1) - a(2,1) * a(4,4) ) &
        + a(1,4) * ( a(2,1) * a(4,2) - a(2,2) * a(4,1) ) &
        ) / det

  b(4,3) = -( &
        + a(1,1) * ( a(2,2) * a(4,3) - a(2,3) * a(4,2) ) &
        + a(1,2) * ( a(2,3) * a(4,1) - a(2,1) * a(4,3) ) &
        + a(1,3) * ( a(2,1) * a(4,2) - a(2,2) * a(4,1) ) &
        ) / det

  b(1,4) = -( &
        + a(1,2) * ( a(2,3) * a(3,4) - a(2,4) * a(3,3) ) &
        + a(1,3) * ( a(2,4) * a(3,2) - a(2,2) * a(3,4) ) &
        + a(1,4) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) &
        ) / det

  b(2,4) = +( &
        + a(1,1) * ( a(2,3) * a(3,4) - a(2,4) * a(3,3) ) &
        + a(1,3) * ( a(2,4) * a(3,1) - a(2,1) * a(3,4) ) &
        + a(1,4) * ( a(2,1) * a(3,3) - a(2,3) * a(3,1) ) &
        ) / det

  b(3,4) = -( &
        + a(1,1) * ( a(2,2) * a(3,4) - a(2,4) * a(3,2) ) &
        + a(1,2) * ( a(2,4) * a(3,1) - a(2,1) * a(3,4) ) &
        + a(1,4) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) &
        ) / det

  b(4,4) = +( &
        + a(1,1) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) &
        + a(1,2) * ( a(2,3) * a(3,1) - a(2,1) * a(3,3) ) &
        + a(1,3) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) &
        ) / det

  return
end
function r8mat_is_binary ( m, n, x )

!*****************************************************************************80
!
!! R8MAT_IS_BINARY is true if an R8MAT only contains 0 and 1 entries.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the dimensions of the array.
!
!    real ( kind = 8 ) X(M,N), the array.
!
!  Output:
!
!    logical R8VEC_IS_BINARY, is true if all entries are 0 or 1.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  logical r8mat_is_binary
  logical value
  real ( kind = 8 ) x(m,n)

  value = .true.

  do j = 1, n
    do i = 1, m

      if ( x(i,j) /= 0.0D+00 .and. x(i,j) /= 1.0D+00 ) then
        value = .false.
        exit
      end if

    end do
  end do

  r8mat_is_binary = value

  return
end
subroutine r8mat_is_identity ( n, a, error_frobenius )

!*****************************************************************************80
!
!! R8MAT_IS_IDENTITY determines if an R8MAT is the identity.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The routine returns the Frobenius norm of A - I.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2007
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix.
!
!    real ( kind = 8 ) A(N,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) ERROR_FROBENIUS, the Frobenius norm
!    of the difference matrix A - I, which would be exactly zero
!    if A were the identity matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) error_frobenius
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) value

  value = 0.0D+00

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        value = value + ( a(i,j) - 1.0D+00 ) ** 2
      else
        value = value + a(i,j) ** 2
      end if
    end do 
  end do

  error_frobenius = sqrt ( error_frobenius )

  error_frobenius = value

  return
end
function r8mat_is_in_01 ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_IS_IN_01 is TRUE if the entries of an R8MAT are in the range [0,1].
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    logical ( kind = 4 ) R8MAT_IS_IN_01, is TRUE if every entry of A is
!    between 0 and 1.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  logical ( kind = 4 ) r8mat_is_in_01

  if ( any ( a(1:m,1:n) < 0.0D+00 .or. 1.0D+00 < a(1:m,1:n) ) ) then
    r8mat_is_in_01 = .false.
  else
    r8mat_is_in_01 = .true.
  end if

  return
end
function r8mat_is_insignificant ( m, n, r, s )

!*****************************************************************************80
!
!! R8MAT_IS_INSIGNIFICANT determines if an R8MAT is insignificant.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the dimension of the matrices.
!
!    real ( kind = 8 ) R(M,N), the array to be compared against.
!
!    real ( kind = 8 ) S(M,N), the array to be compared.
!
!  Output:
!
!    logical ( kind = 4 ) R8MAT_IS_INSIGNIFICANT, is TRUE if S is 
!    insignificant compared to R.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r(m,n)
  logical ( kind = 4 ) r8mat_is_insignificant
  real ( kind = 8 ) s(m,n)
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  logical ( kind = 4 ) value

  value = .true.

  do j = 1, n
    do i = 1, m

      t = r(i,j) + s(i,j)
      tol = epsilon ( r(i,j) ) * abs ( r(i,j) )

      if ( tol < abs ( r(i,j) - t ) ) then 
        value = .false.
        exit
      end if

    end do
  end do
  
  r8mat_is_insignificant = value

  return
end
function r8mat_is_integer ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_IS_INTEGER is true if an R8MAT only contains integer entries.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the dimensions of the array.
!
!    real ( kind = 8 ) A(M,N), the array.
!
!  Output:
!
!    logical R8MAT_IS_INTEGER, is true if all entries are integers.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  logical r8mat_is_integer

  r8mat_is_integer = all ( a(1:m,1:n) == aint ( a(1:m,1:n) ) )

  return
end
subroutine r8mat_is_nonnegative ( m, n, a, ival )

!*****************************************************************************80
!
!! R8MAT_IS_NONNEGATIVE checks whether an R8MAT is nonnegative.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the row and column dimensions of 
!    the matrix.  M and N must be positive.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    logical ( kind = 4 ) R8MAT_IS_NONNEGATIVE:
!    TRUE, the matrix is nonnegative.
!    FALSE, at least one element of A is less than 0.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  logical ( kind = 4 ) ival

  ival = all ( 0.0D+00 <= a(1:m,1:n) )

  return
end
function r8mat_is_significant ( m, n, r, s )

!*****************************************************************************80
!
!! R8MAT_IS_SIGNIFICANT determines if an R8MAT is relatively significant.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 2017
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the dimension of the matrices.
!
!    real ( kind = 8 ) R(M,N), the array to be compared against.
!
!    real ( kind = 8 ) S(M,N), the array to be compared.
!
!  Output:
!
!    logical ( kind = 4 ) R8MAT_IS_SIGNIFICANT, is TRUE if S is 
!    significant relative to R.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r(m,n)
  logical ( kind = 4 ) r8mat_is_significant
  real ( kind = 8 ) s(m,n)
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  logical ( kind = 4 ) value

  value = .false.

  do j = 1, n
    do i = 1, m

      t = r(i,j) + s(i,j)
      tol = epsilon ( r(i,j) ) * abs ( r(i,j) )

      if ( tol < abs ( r(i,j) - t ) ) then 
        value = .true.
        exit
      end if

    end do
  end do
  
  r8mat_is_significant = value

  return
end
subroutine r8mat_is_symmetric ( m, n, a, error_frobenius )

!*****************************************************************************80
!
!! R8MAT_IS_SYMMETRIC checks an R8MAT for symmetry.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2007
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the order of the matrix.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) ERROR_FROBENIUS, measures the 
!    Frobenius norm of ( A - A' ), which would be zero if the matrix
!    were exactly symmetric.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) error_frobenius
  real ( kind = 8 ) value

  if ( m /= n ) then

    value = huge ( value )

  else

    value = sqrt &
      ( &
        sum &
        ( &
          ( &
            abs ( a(1:m,1:n) - transpose ( a(1:m,1:n) ) ) &
          ) ** 2 &
        ) &
      )

  end if

  error_frobenius = value

  return
end
subroutine r8mat_jac ( m, n, eps, fx, x, fprime )

!*****************************************************************************80
!
!! R8MAT_JAC estimates a dense jacobian matrix of the function FX.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    FPRIME(I,J) = d F(I) / d X(J).
!
!    The jacobian is assumed to be dense, and the LINPACK/LAPACK
!    double precision general matrix storage mode ("DGE") is used.
!
!    Forward differences are used, requiring N+1 function evaluations.
!
!    Values of EPS have typically been chosen between
!    sqrt ( EPSMCH ) and sqrt ( sqrt ( EPSMCH ) ) where EPSMCH is the
!    machine tolerance.
!
!    If EPS is too small, then F(X+EPS) will be the same as
!    F(X), and the jacobian will be full of zero entries.
!
!    If EPS is too large, the finite difference estimate will
!    be inaccurate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of functions.
!
!    integer ( kind = 4 ) N, the number of variables.
!
!    real ( kind = 8 ) EPS, a tolerance to be used for shifting the
!    X values during the finite differencing.  No single value
!    of EPS will be reliable for all vectors X and functions FX.
!
!    external FX, the name of the user written
!    routine which evaluates the function at a given point X, of the form:
!      subroutine fx ( m, n, x, f )
!      integer ( kind = 4 ) m
!      integer ( kind = 4 ) n
!      real ( kind = 8 ) f(m)
!      real ( kind = 8 ) x(n)
!      f(1:m) = ...
!      return
!      end
!
!    real ( kind = 8 ) X(N), the point where the jacobian
!    is to be estimated.
!
!  Output:
!
!    real ( kind = 8 ) FPRIME(M,N), the M by N estimated jacobian
!    matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) del
  real ( kind = 8 ) eps
  real ( kind = 8 ) fprime(m,n)
  external fx
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xsave
  real ( kind = 8 ) work1(m)
  real ( kind = 8 ) work2(m)
!
!  Evaluate the function at the base point, X.
!
  call fx ( m, n, x, work2 )
!
!  Now, one by one, vary each component J of the base point X, and
!  estimate DF(I)/DX(J) = ( F(X+) - F(X) )/ DEL.
!
  do j = 1, n

    xsave = x(j)
    del = eps * ( 1.0D+00 + abs ( x(j) ) )
    x(j) = x(j) + del
    call fx ( m, n, x, work1 )
    x(j) = xsave
    fprime(1:m,j) = ( work1(1:m) - work2(1:m) ) / del

  end do

  return
end
subroutine r8mat_kronecker ( m1, n1, a, m2, n2, b, c )

!*****************************************************************************80
!
!! R8MAT_KRONECKER computes the Kronecker product of two R8MAT's.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If A is an M1 by N1 array, and B is an M2 by N2 array, then
!    the Kronecker product of A and B is an M1*M2 by N1*N2 array
!      C(I,J) = A(I1,J1) * B(I2,J2)
!    where
!      I1 =     ( I - 1 ) / M2   + 1
!      I2 = mod ( I - 1,    M2 ) + 1
!      J1 =     ( J - 1 ) / N2   + 1
!      J2 = mod ( J - 1,    N2 ) + 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M1, N1, the order of the first matrix.
!
!    real ( kind = 8 ) A(M1,N1), the first matrix.
!
!    integer ( kind = 4 ) M2, N2, the order of the second matrix.
!
!    real ( kind = 8 ) B(M2,N2), the second matrix.
!
!  Output:
!
!    real ( kind = 8 ) C(M1*M2,N1*N2), the Kronecker product.
!
  implicit none

  integer ( kind = 4 ) m1
  integer ( kind = 4 ) m2
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  real ( kind = 8 ) a(m1,n1)
  real ( kind = 8 ) b(m2,n2)
  real ( kind = 8 ) c(m1*m2,n1*n2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i0
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j0
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2

  do j1 = 1, n1
    do i1 = 1, m1
      i0 = ( i1 - 1 ) * m2
      j0 = ( j1 - 1 ) * n2
      j = j0
      do j2 = 1, n2
        j = j + 1
        i = i0
        do i2 = 1, m2
          i = i + 1
          c(i,j) = a(i1,j1) * b(i2,j2)
        end do
      end do
    end do
  end do

  return
end
subroutine r8mat_l_inverse ( n, a, b )

!*****************************************************************************80
!
!! R8MAT_L_INVERSE inverts a lower triangular R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A lower triangular matrix is a matrix whose only nonzero entries
!    occur on or below the diagonal.
!
!    The inverse of a lower triangular matrix is a lower triangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, number of rows and columns in the matrix.
!
!    real ( kind = 8 ) A(N,N), the lower triangular matrix.
!
!  Output:
!
!    real ( kind = 8 ) B(N,N), the inverse matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do j = 1, n

    do i = 1, n

      if ( i < j ) then
        b(i,j) = 0.0D+00
      else if ( j == i ) then
        b(i,j) = 1.0D+00 / a(i,j)
      else
        b(i,j) = - dot_product ( a(i,1:i-1), b(1:i-1,j) ) / a(i,i)
      end if

    end do
  end do

  return
end
subroutine r8mat_l_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_L_PRINT prints a lower triangular R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Example:
!
!    M = 5, N = 5
!    A = (/ 11, 21, 31, 41, 51, 22, 32, 42, 52, 33, 43, 53, 44, 54, 55 /)
!
!    11
!    21 22
!    31 32 33
!    41 42 43 44
!    51 52 53 54 55
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(*), the M by N matrix.  Only the lower
!    triangular elements are stored, in column major order.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(10)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) jmax
  integer ( kind = 4 ) nn
  integer ( kind = 4 ) size
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  jmax = min ( n, m )

  if ( m <= n ) then
    size = ( m * ( m + 1 ) ) / 2
  else if ( n < m ) then
    size = ( n * ( n + 1 ) ) / 2 + ( m - n ) * n
  end if

  if ( all ( a(1:size) == aint ( a(1:size) ) ) ) then

    nn = 10

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(a8,10i8)' ) '  Col   ', ( j, j = jlo, jhi )
      write ( *, '(a6)' ) '  Row '
      do i = jlo, m
        jhi = min ( jlo + nn - 1, i, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j - 1 ) ) / 2
        end do
        write ( *, '(i8,10i8)' ) i, int ( a(indx(1:jhi+1-jlo)) )
      end do
    end do

  else if ( maxval ( abs ( a(1:size) ) ) < 1000000.0D+00 ) then

    nn = 5

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m - 1, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(8x,5(i8,6x))' ) ( j, j = jlo, jhi )
      write ( *, '(a)' ) ' '
      do i = jlo, m
        jhi = min ( jlo + nn - 1, i, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j - 1 ) ) / 2
        end do
        write ( *, '(i8,5f14.6)' ) i, a(indx(1:jhi+1-jlo))
      end do
    end do

  else

    nn = 5

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m - 1, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(8x,5(i8,6x))' ) ( j, j = jlo, jhi )
      write ( *, '(a)' ) ' '
      do i = jlo, m
        jhi = min ( jlo + nn - 1, i, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j - 1 ) ) / 2
        end do
        write ( *, '(i8,5g14.6)' ) i, a(indx(1:jhi+1-jlo))
      end do
    end do

  end if

  return
end
subroutine r8mat_l_solve ( n, a, b, x )

!*****************************************************************************80
!
!! r8mat_l_solve solves a lower triangular linear system.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns.
!
!    real ( kind = 8 ) A(N,N), the N by N lower triangular matrix.
!
!    real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
!
!  Solve L * x = b.
!
  do i = 1, n
    x(i) = ( b(i) - dot_product ( a(i,1:i-1), x(1:i-1) ) ) / a(i,i)
  end do

  return
end
subroutine r8mat_l1_inverse ( n, a, b )

!*****************************************************************************80
!
!! R8MAT_L1_INVERSE inverts a unit lower triangular R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A unit lower triangular matrix is a matrix with only 1's on the main
!    diagonal, and only 0's above the main diagonal.
!
!    The inverse of a unit lower triangular matrix is also
!    a unit lower triangular matrix.
!
!    This routine can invert a matrix in place, that is, with no extra
!    storage.  If the matrix is stored in A, then the call
!
!      call r8mat_l1_inverse ( n, a, a )
!
!    will result in A being overwritten by its inverse.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, number of rows and columns in the matrix.
!
!    real ( kind = 8 ) A(N,N), the unit lower triangular matrix.
!
!  Output:
!
!    real ( kind = 8 ) B(N,N), the inverse matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do i = 1, n

    do j = 1, n

      if ( i < j ) then
        b(i,j) = 0.0D+00
      else if ( j == i ) then
        b(i,j) = 1.0D+00
      else
        b(i,j) = -dot_product ( a(i,1:i-1), b(1:i-1,j) )
      end if

    end do
  end do

  return
end
subroutine r8mat_lt_solve ( n, a, b, x )

!*****************************************************************************80
!
!! r8mat_lt_solve solves a transposed lower triangular linear system.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    Given the lower triangular matrix A, the linear system to be solved is:
!
!      A' * x = b
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns.
!
!    real ( kind = 8 ) A(N,N), the N by N lower triangular matrix.
!
!    real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
!
!  Solve L'*x = b.
!
  do i = n, 1, -1
    x(i) = ( b(i) - dot_product ( x(i+1:n), a(i+1:n,i) ) ) / a(i,i)
  end do

  return
end
subroutine r8mat_lu ( m, n, a, l, p, u )

!*****************************************************************************80
!
!! R8MAT_LU computes the LU factorization of a rectangular R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The routine is given an M by N matrix A, and produces
!
!      L, an M by M unit lower triangular matrix,
!      U, an M by N upper triangular matrix, and
!      P, an M by M permutation matrix P,
!
!    so that
!
!      A = P' * L * U.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 April 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix to be factored.
!
!  Output:
!
!    real ( kind = 8 ) L(M,M), the M by M unit lower triangular factor.
!
!    real ( kind = 8 ) P(M,M), the M by M permutation matrix.
!
!    real ( kind = 8 ) U(M,N), the M by N upper triangular factor.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ipiv
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) l(m,m)
  real ( kind = 8 ) p(m,m)
  real ( kind = 8 ) pivot
  real ( kind = 8 ) t
  real ( kind = 8 ) u(m,n)
!
!  Initialize:
!
!    U:=A
!    L:=Identity
!    P:=Identity
!
  u(1:m,1:n) = a(1:m,1:n)

  call r8mat_identity ( m, l )

  p(1:m,1:m) = l(1:m,1:m)
!
!  On step J, find the pivot row, IPIV, and the pivot value PIVOT.
!
  do j = 1, min ( m - 1, n )

    pivot = 0.0D+00
    ipiv = 0

    do i = j, m

      if ( pivot < abs ( u(i,j) ) ) then
        pivot = abs ( u(i,j) )
        ipiv = i
      end if

    end do
!
!  Unless IPIV is zero, swap rows J and IPIV.
!
    if ( ipiv /= 0 ) then

      do k = 1, n

        t         = u(j,k)
        u(j,k)    = u(ipiv,k)
        u(ipiv,k) = t

        t         = l(j,k)
        l(j,k)    = l(ipiv,k)
        l(ipiv,k) = t

        t         = p(j,k)
        p(j,k)    = p(ipiv,k)
        p(ipiv,k) = t

      end do
!
!  Zero out the entries in column J, from row J+1 to M.
!
      do i = j + 1, m

        if ( u(i,j) /= 0.0D+00 ) then

          l(i,j) = u(i,j) / u(j,j)

          u(i,j) = 0.0D+00

          u(i,j+1:n) = u(i,j+1:n) - l(i,j) * u(j,j+1:n)

        end if

      end do

    end if

  end do

  return
end
function r8mat_max ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MAX returns the maximum entry of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_MAX, the maximum entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_max

  r8mat_max = maxval ( a(1:m,1:n) )

  return
end
subroutine r8mat_max_columns ( m, n, a, max_col )

!*****************************************************************************80
!
!! R8MAT_MAX_COLUMNS returns the column maximums of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix.
!
!  Output:
!
!    real ( kind = 8 ) MAX_COL(N), the column maximums,
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) max_col(n)

  do j = 1, n
    max_col(j) = maxval ( a(1:m,j) )
  end do

  return
end
subroutine r8mat_max_rows ( m, n, a, max_row )

!*****************************************************************************80
!
!! R8MAT_MAX_ROW returns the row maximums of an R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix.
!
!  Output:
!
!    real ( kind = 8 ) MAX_ROW(M), the row maximums.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) max_row(m)

  do i = 1, m
    max_row(i) = maxval ( a(i,1:n) )
  end do

  return
end
subroutine r8mat_max_index ( m, n, a, i, j )

!*****************************************************************************80
!
!! R8MAT_MAX_INDEX returns the location of the maximum entry of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix.
!
!  Output:
!
!    integer ( kind = 4 ) I, J, the indices of the maximum entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jj

  i = -1
  j = -1

  do jj = 1, n
    do ii = 1, m
      if ( ii == 1 .and. jj == 1 ) then
        i = ii
        j = jj
      else if ( a(i,j) < a(ii,jj) ) then
        i = ii
        j = jj
      end if
    end do
  end do

  return
end
function r8mat_maxcol_minrow ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MAXCOL_MINROW gets the maximum column minimum row of an M by N R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    R8MAT_MAXCOL_MINROW = max ( 1 <= I <= N ) ( min ( 1 <= J <= M ) A(I,J) )
!
!    For a given matrix, R8MAT_MAXCOL_MINROW <= R8MAT_MINROW_MAXCOL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_MAXCOL_MINROW, the maximum column
!    minimum row entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8mat_maxcol_minrow
  real ( kind = 8 ) r8mat_minrow

  r8mat_maxcol_minrow = 0.0D+00

  do i = 1, m

    r8mat_minrow = minval ( a(i,1:n) )

    if ( i == 1 ) then
      r8mat_maxcol_minrow = r8mat_minrow
    else
      r8mat_maxcol_minrow = max ( r8mat_maxcol_minrow, r8mat_minrow )
    end if

  end do

  return
end
function r8mat_maxrow_mincol ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MAXROW_MINCOL gets the maximum row minimum column of an M by N R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    R8MAT_MAXROW_MINCOL = max ( 1 <= J <= N ) ( min ( 1 <= I <= M ) A(I,J) )
!
!    For a given matrix, R8MAT_MAXROW_MINCOL <= R8MAT_MINCOL_MAXROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_MAXROW_MINCOL, the maximum row
!    minimum column entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_maxrow_mincol
  real ( kind = 8 ) r8mat_mincol

  r8mat_maxrow_mincol = 0.0D+00

  do j = 1, n

    r8mat_mincol = minval ( a(1:m,j) )

    if ( j == 1 ) then
      r8mat_maxrow_mincol = r8mat_mincol
    else
      r8mat_maxrow_mincol = max ( r8mat_maxrow_mincol, r8mat_mincol )
    end if

  end do

  return
end
function r8mat_mean ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MEAN returns the mean of an M by N R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 September 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_MEAN, the mean of all the entries.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_mean

  r8mat_mean = sum ( a(1:m,1:n) ) / real ( m * n, kind = 8 )

  return
end
subroutine r8mat_mean_columns ( m, n, a, means )

!*****************************************************************************80
!
!! R8MAT_MEAN_COLUMNS returns the column means of an M by N R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) MEANS(N), the column means.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) means(n)

  do j = 1, n
    means(j) = sum ( a(1:m,j) ) / real ( m, kind = 8 )
  end do

  return
end
subroutine r8mat_mean_rows ( m, n, a, means )

!*****************************************************************************80
!
!! R8MAT_MEAN_ROWS returns the row means of an M by N R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) MEANS(M), the row means.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) means(m)

  do i = 1, m
    means(i) = sum ( a(i,1:n) ) / real ( n, kind = 8 )
  end do

  return
end
function r8mat_min ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MIN returns the minimum entry of an M by N R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_MIN, the minimum entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_min

  r8mat_min = minval ( a(1:m,1:n) )

  return
end
subroutine r8mat_min_columns ( m, n, a, min_col )

!*****************************************************************************80
!
!! R8MAT_MIN_COLUMNS returns the column minimums of an R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix.
!
!  Output:
!
!    real ( kind = 8 ) MIN_COL(N), the column minimums,
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) min_col(n)

  do j = 1, n
    min_col(j) = minval ( a(1:m,j) )
  end do

  return
end
subroutine r8mat_min_rows ( m, n, a, min_row )

!*****************************************************************************80
!
!! R8MAT_MIN_ROW returns the row minimums of an R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix.
!
!  Output:
!
!    real ( kind = 8 ) MIN_ROW(M), the row minimums.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) min_row(m)

  do i = 1, m
    min_row(i) = minval ( a(i,1:n) )
  end do

  return
end
subroutine r8mat_min_index ( m, n, a, i, j )

!*****************************************************************************80
!
!! R8MAT_MIN_INDEX returns the location of the minimum entry of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix.
!
!  Output:
!
!    integer ( kind = 4 ) I, J, the indices of the minimum entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jj

  i = -1
  j = -1

  do jj = 1, n
    do ii = 1, m
      if ( ii == 1 .and. jj == 1 ) then
        i = ii
        j = jj
      else if ( a(ii,jj) < a(i,j) ) then
        i = ii
        j = jj
      end if
    end do
  end do

  return
end
function r8mat_mincol_maxrow ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MINCOL_MAXROW gets the minimum column maximum row of an M by N R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    R8MAT_MINCOL_MAXROW = min ( 1 <= I <= N ) ( max ( 1 <= J <= M ) A(I,J) )
!
!    For a given matrix, R8MAT_MAXROW_MINCOL <= R8MAT_MINCOL_MAXROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_MINCOL_MAXROW, the minimum column
!    maximum row entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8mat_mincol_maxrow
  real ( kind = 8 ) r8mat_maxrow

  r8mat_mincol_maxrow = 0.0D+00

  do i = 1, m

    r8mat_maxrow = maxval ( a(i,1:n) )

    if ( i == 1 ) then
      r8mat_mincol_maxrow = r8mat_maxrow
    else
      r8mat_mincol_maxrow = min ( r8mat_mincol_maxrow, r8mat_maxrow )
    end if

  end do

  return
end
function r8mat_minrow_maxcol ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_MINROW_MAXCOL gets the minimum row maximum column of an M by N R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    R8MAT_MINROW_MAXCOL = min ( 1 <= J <= N ) ( max ( 1 <= I <= M ) A(I,J) )
!
!    For a given matrix, R8MAT_MAXCOL_MINROW <= R8MAT_MINROW_MAXCOL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_MINROW_MAXCOL, the minimum row
!    maximum column entry of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_minrow_maxcol
  real ( kind = 8 ) r8mat_maxcol

  r8mat_minrow_maxcol = 0.0D+00

  do j = 1, n

    r8mat_maxcol = maxval ( a(1:m,j) )

    if ( j == 1 ) then
      r8mat_minrow_maxcol = r8mat_maxcol
    else
      r8mat_minrow_maxcol = min ( r8mat_minrow_maxcol, r8mat_maxcol )
    end if

  end do

  return
end
subroutine r8mat_minvm ( n1, n2, a, b, c )

!*****************************************************************************80
!
!! R8MAT_MINVM computes inverse(A) * B for R8MAT's.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N1, N2, the order of the matrices.
!
!    real ( kind = 8 ) A(N1,N1), B(N1,N2), the matrices.
!
!  Output:
!
!    real ( kind = 8 ) C(N1,N2), the result, C = inverse(A) * B.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  real ( kind = 8 ) a(n1,n1)
  real ( kind = 8 ) alu(n1,n1)
  real ( kind = 8 ) b(n1,n2)
  real ( kind = 8 ) c(n1,n2)
  integer ( kind = 4 ) info

  alu(1:n1,1:n1) = a(1:n1,1:n1)
  c(1:n1,1:n2) = b(1:n1,1:n2)

  call r8mat_fss ( n1, alu, n2, c, info )
 
  if ( info /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_MINVM - Fatal error!'
    write ( *, '(a)' ) '  The matrix A was numerically singular.'
    stop 1
  end if

  return
end
subroutine r8mat_mm ( n1, n2, n3, a, b, c )

!*****************************************************************************80
!
!! R8MAT_MM multiplies two R8MAT's.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In FORTRAN90, this operation is more efficiently done by the
!    command:
!
!      C(1:N1,1:N3) = MATMUL ( A(1:N1,1;N2), B(1:N2,1:N3) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N1, N2, N3, the order of the matrices.
!
!    real ( kind = 8 ) A(N1,N2), B(N2,N3), the matrices to multiply.
!
!  Output:
!
!    real ( kind = 8 ) C(N1,N3), the product matrix C = A * B.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3

  real ( kind = 8 ) a(n1,n2)
  real ( kind = 8 ) b(n2,n3)
  real ( kind = 8 ) c(n1,n3)

  c(1:n1,1:n3) = matmul ( a(1:n1,1:n2), b(1:n2,1:n3) )

  return
end
subroutine r8mat_mmt ( n1, n2, n3, a, b, c )

!*****************************************************************************80
!
!! R8MAT_MMT computes C = A * B' for two R8MAT's.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In FORTRAN90, this operation is more efficiently done by the
!    command:
!
!      C(1:N1,1:N3) = matmul ( A(1:N1,1;N2) ), transpose ( B(1:N3,1:N2) ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 November 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N1, N2, N3, the order of the matrices.
!
!    real ( kind = 8 ) A(N1,N2), B(N3,N2), the matrices to multiply.
!
!  Output:
!
!    real ( kind = 8 ) C(N1,N3), the product matrix C = A * B.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3

  real ( kind = 8 ) a(n1,n2)
  real ( kind = 8 ) b(n3,n2)
  real ( kind = 8 ) c(n1,n3)

  c(1:n1,1:n3) = matmul ( &
                                      a(1:n1,1:n2), &
                          transpose ( b(1:n3,1:n2) ) &
                        )

  return
end
subroutine r8mat_mtm ( n1, n2, n3, a, b, c )

!*****************************************************************************80
!
!! R8MAT_MTM computes C = A' * B for two R8MAT's.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In FORTRAN90, this operation is more efficiently done by the
!    command:
!
!      C(1:N1,1:N3) = matmul ( transpose ( A(1:N2,1;N1) ), B(1:N2,1:N3) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N1, N2, N3, the order of the matrices.
!
!    real ( kind = 8 ) A(N2,N1), B(N2,N3), the matrices to multiply.
!
!  Output:
!
!    real ( kind = 8 ) C(N1,N3), the product matrix C = A * B.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3

  real ( kind = 8 ) a(n2,n1)
  real ( kind = 8 ) b(n2,n3)
  real ( kind = 8 ) c(n1,n3)

  c(1:n1,1:n3) = matmul ( transpose ( a(1:n2,1:n1) ), b(1:n2,1:n3) )

  return
end
subroutine r8mat_mtv ( m, n, a, x, y )

!*****************************************************************************80
!
!! R8MAT_MTV multiplies a transposed matrix times a vector
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix.
!
!    real ( kind = 8 ) X(M), the vector to be multiplied by A.
!
!  Output:
!
!    real ( kind = 8 ) Y(N), the product A'*X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) x(m)
  real ( kind = 8 ) y(n)

  y(1:n) = matmul ( transpose ( a(1:m,1:n) ), x(1:m) )

  return
end
subroutine r8mat_mv ( m, n, a, x, y )

!*****************************************************************************80
!
!! R8MAT_MV multiplies a matrix times a vector.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In FORTRAN90, this operation can be more efficiently carried
!    out by the command
!
!      Y(1:M) = MATMUL ( A(1:M,1:N), X(1:N) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix.
!
!    real ( kind = 8 ) X(N), the vector to be multiplied by A.
!
!  Output:
!
!    real ( kind = 8 ) Y(M), the product A*X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(m)

  y(1:m) = matmul ( a(1:m,1:n), x(1:n) )

  return
end
subroutine r8mat_nint ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NINT rounds the entries of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) A(M,N), the matrix to be NINT'ed.
!
!  Output:
!
!    real ( kind = 8 ) A(M,N), the NINT'ed matrix.
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)

  a(1:m,1:n) = real ( nint ( a(1:m,1:n) ), kind = 8 )

  return
end
function r8mat_nonzeros ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NONZEROS counts the nonzeros in an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
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
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    integer ( kind = 4 ) R8MAT_NONZEROS, the number of nonzeros.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) r8mat_nonzeros
  integer ( kind = 4 ) value

  value = 0
  do j = 1, n
    do i = 1, m
      if ( a(i,j) /= 0.0D+00 ) then
        value = value + 1
      end if
    end do
  end do

  r8mat_nonzeros = value

  return
end
function r8mat_norm_eis ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NORM_EIS returns the EISPACK norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The EISPACK norm is defined as:
!
!      R8MAT_NORM_EIS =
!        sum ( 1 <= I <= M ) sum ( 1 <= J <= N ) abs ( A(I,J) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix whose EISPACK norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_NORM_EIS, the EISPACK norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_norm_eis

  r8mat_norm_eis = sum ( abs ( a(1:m,1:n) ) )

  return
end
function r8mat_norm_fro ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NORM_FRO returns the Frobenius norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Frobenius norm is defined as
!
!      R8MAT_NORM_FRO = sqrt (
!        sum ( 1 <= I <= M ) sum ( 1 <= j <= N ) A(I,J) * A(I,J) )
!
!    The matrix Frobenius norm is not derived from a vector norm, but
!    is compatible with the vector L2 norm, so that:
!
!      r8vec_norm_l2 ( A * x ) <= r8mat_norm_fro ( A ) * r8vec_norm_l2 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix whose Frobenius
!    norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_NORM_FRO, the Frobenius norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_norm_fro

  r8mat_norm_fro = sqrt ( sum ( a(1:m,1:n) ** 2 ) )

  return
end
function r8mat_norm_fro_affine ( m, n, a1, a2 )

!*****************************************************************************80
!
!! R8MAT_NORM_FRO_AFFINE returns the Frobenius norm of an R8MAT difference.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Frobenius norm is defined as
!
!      R8MAT_NORM_FRO = sqrt (
!        sum ( 1 <= I <= M ) sum ( 1 <= j <= N ) A(I,J) * A(I,J) )
!
!    The matrix Frobenius norm is not derived from a vector norm, but
!    is compatible with the vector L2 norm, so that:
!
!      r8vec_norm_l2 ( A * x ) <= r8mat_norm_fro ( A ) * r8vec_norm_l2 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows.
!
!    integer ( kind = 4 ) N, the number of columns.
!
!    real ( kind = 8 ) A1(M,N), A2(M,N), the matrices for whose 
!    difference the Frobenius norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_NORM_FRO_AFFINE, the Frobenius 
!    norm of A1 - A2.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(m,n)
  real ( kind = 8 ) a2(m,n)
  real ( kind = 8 ) r8mat_norm_fro_affine

  r8mat_norm_fro_affine = sqrt ( sum ( ( a1(1:m,1:n) - a2(1:m,1:n) ) ** 2 ) )

  return
end
function r8mat_norm_l1 ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NORM_L1 returns the matrix L1 norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The matrix L1 norm is defined as:
!
!      R8MAT_NORM_L1 = max ( 1 <= J <= N )
!        sum ( 1 <= I <= M ) abs ( A(I,J) ).
!
!    The matrix L1 norm is derived from the vector L1 norm, and
!    satisifies:
!
!      r8vec_norm_l1 ( A * x ) <= r8mat_norm_l1 ( A ) * r8vec_norm_l1 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix whose L1 norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_NORM_L1, the L1 norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) col_sum
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_norm_l1

  r8mat_norm_l1 = 0.0D+00

  do j = 1, n
    col_sum = sum ( abs ( a(1:m,j) ) )
    r8mat_norm_l1 = max ( r8mat_norm_l1, col_sum )
  end do

  return
end
function r8mat_norm_l2 ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NORM_L2 returns the matrix L2 norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The matrix L2 norm is defined as:
!
!      R8MAT_NORM_L2 = sqrt ( max ( 1 <= I <= M ) LAMBDA(I) )
!
!    where LAMBDA contains the eigenvalues of A * A'.
!
!    The matrix L2 norm is derived from the vector L2 norm, and
!    satisifies:
!
!      r8vec_norm_l2 ( A * x ) <= r8mat_norm_l2 ( A ) * r8vec_norm_l2 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix whose L2 norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_NORM_L2, the L2 norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) b(m,m)
  real ( kind = 8 ) diag(m)
  real ( kind = 8 ) r8mat_norm_l2
!
!  Compute B = A * A'.
!
  b(1:m,1:m) = matmul ( a(1:m,1:n), transpose ( a(1:m,1:n) ) )
!
!  Diagonalize B.
!
  call r8mat_symm_jacobi ( m, b )
!
!  Find the maximum eigenvalue, and take its square root.
!
  call r8mat_diag_get_vector ( m, b, diag )

  r8mat_norm_l2 = sqrt ( maxval ( diag(1:m) ) )

  return
end
function r8mat_norm_li ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_NORM_LI returns the matrix Loo norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The matrix Loo norm is defined as:
!
!      R8MAT_NORM_LI =  max ( 1 <= I <= M ) sum ( 1 <= J <= N ) abs ( A(I,J) ).
!
!    The matrix Loo norm is derived from the vector Loo norm,
!    and satisifies:
!
!      r8vec_norm_li ( A * x ) <= r8mat_norm_li ( A ) * r8vec_norm_li ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix whose Loo
!    norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_NORM_LI, the Loo norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8mat_norm_li
  real ( kind = 8 ) row_sum

  r8mat_norm_li = 0.0D+00

  do i = 1, m
    row_sum = sum ( abs ( a(i,1:n) ) )
    r8mat_norm_li = max ( r8mat_norm_li, row_sum )
  end do

  return
end
subroutine r8mat_normal_01 ( m, n, seed, r )

!*****************************************************************************80
!
!! R8MAT_NORMAL_01 returns a unit pseudonormal R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, 1969, pages 136-143.
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On SEED has been updated.
!
!  Output:
!
!    real ( kind = 8 ) R(M,N), the array of pseudonormal values.
!
!    integer ( kind = 4 ) SEED, an updated seed value.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  call r8vec_normal_01 ( m * n, seed, r )

  return
end
subroutine r8mat_nullspace ( m, n, a, nullspace_size, nullspace )

!*****************************************************************************80
!
!! R8MAT_NULLSPACE computes the nullspace of a matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    Let A be an MxN matrix.
!
!    If X is an N-vector, and A*X = 0, then X is a null vector of A.
!
!    The set of all null vectors of A is called the nullspace of A.
!
!    The 0 vector is always in the null space.
!
!    If the 0 vector is the only vector in the nullspace of A, then A
!    is said to have maximum column rank.  (Because A*X=0 can be regarded
!    as a linear combination of the columns of A).  In particular, if A
!    is square, and has maximum column rank, it is nonsingular.
!
!    The dimension of the nullspace is the number of linearly independent
!    vectors that span the nullspace.  If A has maximum column rank,
!    its nullspace has dimension 0.
!
!    This routine uses the reduced row echelon form of A to determine
!    a set of NULLSPACE_SIZE independent null vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix A.
!
!    real ( kind = 8 ) A(M,N), the matrix to be analyzed.
!
!    integer ( kind = 4 ) NULLSPACE_SIZE, the size of the nullspace.
!
!  Output:
!
!    real ( kind = 8 ) NULLSPACE(N,NULLSPACE_SIZE), vectors that
!    span the nullspace.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nullspace_size

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) col(n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  real ( kind = 8 ) nullspace(n,nullspace_size)
  integer ( kind = 4 ) row(m)
  real ( kind = 8 ) rref(m,n)
!
!  Make a copy of A.
!
  rref(1:m,1:n) = a(1:m,1:n)
!
!  Get the reduced row echelon form of A.
!
  call r8mat_rref ( m, n, rref, det )
!
!  Note in ROW the columns of the leading nonzeros.
!  COL(J) = +J if there is a leading 1 in that column, and -J otherwise.
!
  row(1:m) = 0

  do j = 1, n
    col(j) = - j
  end do

  do i = 1, m
    do j = 1, n
      if ( rref(i,j) == 1.0D+00 ) then
        row(i) = j
        col(j) = j
        exit
      end if
    end do
  end do

  nullspace(1:n,1:nullspace_size) = 0.0D+00

  j2 = 0
!
!  If column J does not contain a leading 1, then it contains
!  information about a null vector.
!
  do j = 1, n

    if ( col(j) < 0 ) then

      j2 = j2 + 1

      do i = 1, m
        if ( rref(i,j) /= 0.0D+00 ) then
          i2 = row(i)
          nullspace(i2,j2) = - rref(i,j)
        end if
      end do

      nullspace(j,j2) = 1.0D+00

    end if

  end do

  return
end
subroutine r8mat_nullspace_size ( m, n, a, nullspace_size )

!*****************************************************************************80
!
!! R8MAT_NULLSPACE_SIZE computes the size of the nullspace of a matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    Let A be an MxN matrix.
!
!    If X is an N-vector, and A*X = 0, then X is a null vector of A.
!
!    The set of all null vectors of A is called the nullspace of A.
!
!    The 0 vector is always in the null space.
!
!    If the 0 vector is the only vector in the nullspace of A, then A
!    is said to have maximum column rank.  (Because A*X=0 can be regarded
!    as a linear combination of the columns of A).  In particular, if A
!    is square, and has maximum column rank, it is nonsingular.
!
!    The dimension of the nullspace is the number of linearly independent
!    vectors that span the nullspace.  If A has maximum column rank,
!    its nullspace has dimension 0.
!
!    This routine ESTIMATES the dimension of the nullspace.  Cases of
!    singularity that depend on exact arithmetic will probably be missed.
!
!    The nullspace will be estimated by counting the leading 1's in the
!    reduced row echelon form of A, and subtracting this from N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix A.
!
!    real ( kind = 8 ) A(M,N), the matrix to be analyzed.
!
!  Output:
!
!    integer ( kind = 4 ) NULLSPACE_SIZE, the estimated size
!    of the nullspace.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) leading
  integer ( kind = 4 ) nullspace_size
  real ( kind = 8 ) rref(m,n)
!
!  Get the reduced row echelon form of A.
!
  rref(1:m,1:n) = a(1:m,1:n)

  call r8mat_rref ( m, n, rref, det )
!
!  Count the leading 1's in A.
!
  leading = 0
  do i = 1, m
    do j = 1, n
      if ( rref(i,j) == 1.0D+00 ) then
        leading = leading + 1
        exit
      end if
    end do
  end do

  nullspace_size = n - leading

  return
end
subroutine r8mat_orth_uniform ( n, seed, q )

!*****************************************************************************80
!
!! R8MAT_ORTH_UNIFORM returns a random orthogonal R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    Thanks to Eugene Petrov, B I Stepanov Institute of Physics,
!    National Academy of Sciences of Belarus, for convincingly
!    pointing out the severe deficiencies of an earlier version of
!    this routine.
!
!    Essentially, the computation involves saving the Q factor of the
!    QR factorization of a matrix whose entries are normally distributed.
!    However, it is only necessary to generate this matrix a column at
!    a time, since it can be shown that when it comes time to annihilate
!    the subdiagonal elements of column K, these (transformed) elements of
!    column K are still normally distributed random values.  Hence, there
!    is no need to generate them at the beginning of the process and
!    transform them K-1 times.
!
!    For computational efficiency, the individual Householder transformations
!    could be saved, as recommended in the reference, instead of being
!    accumulated into an explicit matrix format.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Pete Stewart,
!    Efficient Generation of Random Orthogonal Matrices With an Application
!    to Condition Estimators,
!    SIAM Journal on Numerical Analysis,
!    Volume 17, Number 3, June 1980, pages 403-409.
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of A.
!
!    integer ( kind = 4 ) SEED, a seed for the random number generator.
!
!  Output:
!
!    real ( kind = 8 ) Q(N,N), the orthogonal matrix.
!
!    integer ( kind = 4 ) SEED, an updated seed value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a_col(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) q(n,n)
  real ( kind = 8 ) r8_normal_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) v(n)
!
!  Start with Q = the identity matrix.
!
  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        q(i,j) = 1.0D+00
      else
        q(i,j) = 0.0D+00
      end if
    end do
  end do
!
!  Now behave as though we were computing the QR factorization of
!  some other random matrix A.  Generate the N elements of the first column,
!  compute the Householder matrix H1 that annihilates the subdiagonal elements,
!  and set Q := Q * H1' = Q * H.
!
!  On the second step, generate the lower N-1 elements of the second column,
!  compute the Householder matrix H2 that annihilates them,
!  and set Q := Q * H2' = Q * H2 = H1 * H2.
!
!  On the N-1 step, generate the lower 2 elements of column N-1,
!  compute the Householder matrix HN-1 that annihilates them, and
!  and set Q := Q * H(N-1)' = Q * H(N-1) = H1 * H2 * ... * H(N-1).
!  This is our random orthogonal matrix.
!
  do j = 1, n - 1
!
!  Set the vector that represents the J-th column to be annihilated.
!
    a_col(1:j-1) = 0.0D+00

    do i = j, n
      a_col(i) = r8_normal_01 ( seed )
    end do
!
!  Compute the vector V that defines a Householder transformation matrix
!  H(V) that annihilates the subdiagonal elements of A.
!
    call r8vec_house_column ( n, a_col, j, v )
!
!  Postmultiply the matrix Q by H'(V) = H(V).
!
    call r8mat_house_axh ( n, q, v, q )

  end do

  return
end
subroutine r8mat_plot ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_PLOT "plots" an R8MAT, with an optional title.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character r8mat_plot_symbol
  character ( len = 70 ) string
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  do jlo = 1, n, 70
    jhi = min ( jlo + 70-1, n )
    write ( *, '(a)' ) ' '
    write ( *, '(8x,2x,70i1)' ) ( mod ( j, 10 ), j = jlo, jhi )
    write ( *, '(a)' ) ' '

    do i = 1, m
      do j = jlo, jhi
        string(j+1-jlo:j+1-jlo) = r8mat_plot_symbol ( a(i,j) )
      end do
      write ( *, '(i8,2x,a)' ) i, string(1:jhi+1-jlo)
    end do
  end do

  return
end
function r8mat_plot_symbol ( r )

!*****************************************************************************80
!
!! R8MAT_PLOT_SYMBOL returns a symbol for an element of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) R, a value whose symbol is desired.
!
!  Output:
!
!    character R8MAT_PLOT_SYMBOL, is
!    '-' if R is negative,
!    '0' if R is zero,
!    '+' if R is positive.
!
  implicit none

  character r8mat_plot_symbol
  real ( kind = 8 ) r

  if ( r < 0.0D+00 ) then
    r8mat_plot_symbol = '-'
  else if ( r == 0.0D+00 ) then
    r8mat_plot_symbol = '0'
  else if ( 0.0D+00 < r ) then
    r8mat_plot_symbol = '+'
  end if

  return
end
subroutine r8mat_poly_char ( n, a, p )

!*****************************************************************************80
!
!! R8MAT_POLY_CHAR computes the characteristic polynomial of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix A.
!
!    real ( kind = 8 ) A(N,N), the N by N matrix.
!
!  Output:
!
!    real ( kind = 8 ) P(0:N), the coefficients of the characteristic
!    polynomial of A.  P(N) contains the coefficient of X^N
!    (which will be 1), P(I) contains the coefficient of X^I,
!    and P(0) contains the constant term.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order
  real ( kind = 8 ) p(0:n)
  real ( kind = 8 ) r8mat_trace
  real ( kind = 8 ) trace
  real ( kind = 8 ) work1(n,n)
  real ( kind = 8 ) work2(n,n)
!
!  Initialize WORK1 to the identity matrix.
!
  call r8mat_identity ( n, work1 )

  p(n) = 1.0D+00

  do order = n - 1, 0, -1
!
!  Work2 = A * WORK1.
!
    work2(1:n,1:n) = matmul ( a(1:n,1:n), work1(1:n,1:n) )
!
!  Take the trace.
!
    trace = r8mat_trace ( n, work2 )
!
!  P(ORDER) = -Trace ( WORK2 ) / ( N - ORDER )
!
    p(order) = -trace / real ( n - order, kind = 8 )
!
!  WORK1 := WORK2 + P(ORDER) * Identity.
!
    work1(1:n,1:n) = work2(1:n,1:n)

    do i = 1, n
      work1(i,i) = work1(i,i) + p(order)
    end do

  end do

  return
end
subroutine r8mat_power ( n, a, npow, b )

!*****************************************************************************80
!
!! R8MAT_POWER computes a nonnegative power of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The algorithm is:
!
!      B = I
!      do NPOW times:
!        B = A * B
!      end
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of A.
!
!    real ( kind = 8 ) A(N,N), the matrix to be raised to a power.
!
!    integer ( kind = 4 ) NPOW, the power to which A is to be raised.
!    NPOW must be nonnegative.
!
!  Output:
!
!    real ( kind = 8 ) B(N,N), the value of A^NPOW.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  integer ( kind = 4 ) ipow
  integer ( kind = 4 ) npow

  if ( npow < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_POWER - Fatal error!'
    write ( *, '(a)' ) '  Input value of NPOW < 0.'
    write ( *, '(a,i8)' ) '  NPOW = ', npow
    stop 1
  end if

  call r8mat_identity ( n, b )

  do ipow = 1, npow
    b(1:n,1:n) = matmul ( a(1:n,1:n), b(1:n,1:n) )
  end do

  return
end
subroutine r8mat_power_method ( n, a, r, v )

!*****************************************************************************80
!
!! R8MAT_POWER_METHOD applies the power method to an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If the power method has not converged, then calling the routine
!    again immediately with the output from the previous call will
!    continue the iteration.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of A.
!
!    real ( kind = 8 ) A(N,N), the matrix.
!
!    real ( kind = 8 ) V(N), an estimate for the eigenvector. 
!
!  Output:
!
!    real ( kind = 8 ) R, the estimated eigenvalue.
!
!    real ( kind = 8 ) V(N), an improved estimate for the
!    eigenvector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) av(n)
  real ( kind = 8 ) eps
  integer ( kind = 4 ) it
  real ( kind = 8 ), parameter :: it_eps = 0.0001D+00
  integer ( kind = 4 ), parameter :: it_max = 100
  integer ( kind = 4 ), parameter :: it_min = 10
  integer ( kind = 4 ) j
  real ( kind = 8 ) r
  real ( kind = 8 ) r2
  real ( kind = 8 ) r_old
  real ( kind = 8 ) v(n)

  eps = sqrt ( epsilon ( 1.0D+00 ) )

  r = sqrt ( sum ( v(1:n) ** 2 ) )

  if ( r == 0.0D+00 ) then
    v(1:n) = 1.0D+00
    r = sqrt ( real ( n, kind = 8 ) )
  end if

  v(1:n) = v(1:n) / r

  do it = 1, it_max

    av(1:n) = matmul ( a(1:n,1:n), v(1:n) )

    r_old = r
    r = sqrt ( sum ( av(1:n) ** 2 ) )

    if ( it_min < it ) then
      if ( abs ( r - r_old ) <= it_eps * ( 1.0D+00 + abs ( r ) ) ) then
        exit
      end if
    end if

    v(1:n) = av(1:n)

    if ( r /= 0.0D+00 ) then
      v(1:n) = v(1:n) / r
    end if
!
!  Perturb V a bit, to avoid cases where the initial guess is exactly
!  the eigenvector of a smaller eigenvalue.
!
    if ( it < it_max / 2 ) then
      j = 1 + mod ( it - 1, n )
      v(j) = v(j) + eps * ( 1.0D+00 + abs ( v(j) ) )
      r2 = sqrt ( sum ( v(1:n) ** 2 ) )
      v(1:n) = v(1:n) / r2
    end if

  end do

  return
end
subroutine r8mat_print ( m, n, a, title )

!*****************************************************************************80
!
!! r8mat_print() prints a real matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! r8mat_print_some prints some of a real matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

    j2hi = j2lo + incx - 1
    j2hi = min ( j2hi, n )
    j2hi = min ( j2hi, jhi )

    inc = j2hi + 1 - j2lo

    write ( *, '(a)' ) ' '

    do j = j2lo, j2hi
      j2 = j + 1 - j2lo
      write ( ctemp(j2), '(i8,6x)' ) j
    end do

    write ( *, '(''  Col   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Row'
    write ( *, '(a)' ) ' '

    i2lo = max ( ilo, 1 )
    i2hi = min ( ihi, m )

    do i = i2lo, i2hi

      do j2 = 1, inc

        j = j2lo - 1 + j2

        write ( ctemp(j2), '(g14.6)' ) a(i,j)

      end do

      write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

    end do

  end do

  return
end
subroutine r8mat_print2 ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_PRINT2 prints an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2002
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows of A.
!
!    integer ( kind = 4 ) N, the number of columns of A.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix to be printed.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) amin
  integer ( kind = 4 ) i
  character ( len = 10 ) iform
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  logical ( kind = 4 ) integ
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) lmax
  integer ( kind = 4 ) npline
  real ( kind = 8 ) r8_log_10
!
!  Check if all entries are integral.
!
  integ = .true.

  do i = 1, m
    do j = 1, n

      if ( integ ) then
        if ( a(i,j) /= real ( int ( a(i,j) ), kind = 8 ) ) then
          integ = .false.
        end if
      end if

    end do
  end do
!
!  Find the maximum and minimum entries.
!
  amax = maxval ( a(1:m,1:n) )
  amin = minval ( a(1:m,1:n) )
!
!  Use the information about the maximum size of an entry to
!  compute an intelligent format for use with integer entries.
!
!  Later, we might also do this for real matrices.
!
  lmax = int ( r8_log_10 ( amax ) )

  if ( integ ) then
    npline = 79 / ( lmax + 3 )
    write ( iform, '(''('',i2,''I'',i2,'')'')' ) npline, lmax+3
  else
    npline = 5
    iform = ' '
  end if
!
!  Print a scalar quantity.
!
  if ( m == 1 .and. n == 1 ) then

    if ( integ ) then
      write ( *, iform ) int ( a(1,1) )
    else
      write ( *, '(2x,g14.6)' ) a(1,1)
    end if
!
!  Column vector of length M,
!
  else if ( n == 1 ) then

    do ilo = 1, m, npline

      ihi = min ( ilo+npline-1, m )

      if ( integ ) then
        write ( *, iform ) ( int ( a(i,1) ), i = ilo, ihi )
      else
        write ( *, '(2x,5g14.6)' ) a(ilo:ihi,1)
      end if

    end do
!
!  Row vector of length N,
!
  else if ( m == 1 ) then

    do jlo = 1, n, npline

      jhi = min ( jlo+npline-1, n )

      if ( integ ) then
        write ( *, iform ) int ( a(1,jlo:jhi) )
      else
        write ( *, '(2x,5g14.6)' ) a(1,jlo:jhi)
      end if

    end do
!
!  M by N Array
!
  else

    do jlo = 1, n, npline

      jhi = min ( jlo+npline-1, n )

      if ( npline < n ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8,a,i8)' ) 'Matrix columns ', jlo, ' to ', jhi
        write ( *, '(a)' ) ' '
      end if

      do i = 1, m

        if ( integ ) then
          write ( *, iform ) int ( a(i,jlo:jhi) )
        else
          write ( *, '(2x,5g14.6)' ) a(i,jlo:jhi)
        end if

      end do
    end do

  end if

  return
end
function r8mat_product_elementwise ( m, n, a, b )

!*****************************************************************************80
!
!! R8MAT_PRODUCT_ELEMENTWISE returns the elementwise produce to two R8MAT's.
!
!  Example:
!
!    A = [ 1, 2, 3;    B = [ 1, 3, 5;    product = 86
!          4, 5, 6 ]         2, 4, 6 ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows.
!
!    integer ( kind = 4 ) N, the number of columns.
!
!    real ( kind = 8 ) A(M,N), B(M,N), the two matrices.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_PRODUCT_ELEMENTWISE, the elementwise 
!    product of A and B.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) b(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r8mat_product_elementwise
  real ( kind = 8 ) value

  value = 0.0D+00
  do j = 1, n
    do i = 1, m
      value = value + a(i,j) * b(i,j)
    end do
  end do
  
  r8mat_product_elementwise = value

  return
end
subroutine r8mat_ref ( m, n, a, det )

!*****************************************************************************80
!
!! R8MAT_REF computes the row echelon form of a matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A matrix is in row echelon form if:
!
!    * The first nonzero entry in each row is 1.
!
!    * The leading 1 in a given row occurs in a column to
!      the right of the leading 1 in the previous row.
!
!    * Rows which are entirely zero must occur last.
!
!  Example:
!
!    Input matrix:
!
!     1.0  3.0  0.0  2.0  6.0  3.0  1.0
!    -2.0 -6.0  0.0 -2.0 -8.0  3.0  1.0
!     3.0  9.0  0.0  0.0  6.0  6.0  2.0
!    -1.0 -3.0  0.0  1.0  0.0  9.0  3.0
!
!    Output matrix:
!
!     1.0  3.0  0.0  2.0  6.0  3.0  1.0
!     0.0  0.0  0.0  1.0  2.0  4.5  1.5
!     0.0  0.0  0.0  0.0  0.0  1.0  0.3
!     0.0  0.0  0.0  0.0  0.0  0.0  0.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Charles Cullen,
!    An Introduction to Numerical Linear Algebra,
!    PWS Publishing Company, 1994,
!    ISBN: 978-0534936903,
!    LC: QA185.D37.C85.
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix A.
!
!    real ( kind = 8 ) A(M,N).  On the matrix to be analyzed.  
!
!  Output:
!
!    real ( kind = 8 ) A(M,N), the REF form of the matrix.
!
!    real ( kind = 8 ) DET, the pseudo-determinant.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lead
  integer ( kind = 4 ) r
  real ( kind = 8 ) r8_epsilon
  real ( kind = 8 ) temp
  real ( kind = 8 ) tol

  det = 1.0D+00
  tol = r8_epsilon ( ) * sum ( abs ( a(1:m,1:n) ) )
  lead = 1

  do r = 1, m

    if ( n < lead ) then
      exit
    end if

    i = r

    do while ( abs ( a(i,lead) ) <= tol )

      i = i + 1

      if ( m < i ) then
        i = r
        lead = lead + 1
        if ( n < lead ) then
          lead = -1
          exit
        end if
      end if

    end do

    if ( lead < 0 ) then
      exit
    end if

    do j = 1, n
      temp   = a(i,j)
      a(i,j) = a(r,j)
      a(r,j) = temp
    end do

    det = det * a(r,lead)
    a(r,1:n) = a(r,1:n) / a(r,lead)

    do i = r + 1, m
      a(i,1:n) = a(i,1:n) - a(i,lead) * a(r,1:n)
    end do

    lead = lead + 1

  end do

  return
end
function r8mat_rms ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_RMS returns the RMS norm of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The matrix RMS norm is defined as:
!
!      R8MAT_RMS = sqrt ( 
!        sum ( 1 <= I <= M ) sum ( 1 <= J <= N ) A(I,J)^2 / M / N ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 December 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the dimensions of the matrix.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_RMS, the RMS norm of A.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_rms

  r8mat_rms = sqrt ( sum ( a(1:m,1:n) ** 2 ) &
    / real ( m, kind = 8 ) / real ( n, kind = 8 ) )

  return
end
subroutine r8mat_row_copy ( m, n, i, v, a )

!*****************************************************************************80
!
!! R8MAT_ROW_COPY copies a vector into a row of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the order of the matrix.
!
!    integer ( kind = 4 ) I, the index of the row.
!    1 <= I <= M.
!
!    real ( kind = 8 ) V(N), the row to be copied.
!
!    real ( kind = 8 ) A(M,N), the matrix into which
!    the row is to be copied.
!
!  Output:
!
!    real ( kind = 8 ) A(M,N), the modified matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)

  a(i,1:n) = v(1:n)

  return
end
subroutine r8mat_row_set ( i, r, m, n, a )

!*****************************************************************************80
!
!! R8MAT_ROW_SET copies a vector into a row of an R8MAT.
!
!  Discussion:
!
!    Because I try to avoid using "leading dimensions", which allow
!    a user to set aside too much space for an array, but then
!    still put things in the right place, I need to use a routine
!    like this when I occasionally have to deal with arrays that
!    are not "tight".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) I, the row index.
!
!    real ( kind = 8 ) R(N), the vector.
!
!    integer ( kind = 4 ) M, N, the number of rows and 
!    columns of the matrix.
!
!    real ( kind = 8 ) A(M,N), the matrix to be updated.
!
!  Output:
!
!    real ( kind = 8 ) A(M,N), the updated matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r(n)

  a(i,1:n) = r(1:n)

  return
end
subroutine r8mat_rref ( m, n, a, det )

!*****************************************************************************80
!
!! R8MAT_RREF computes the reduced row echelon form of a matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A matrix is in row echelon form if:
!
!    * The first nonzero entry in each row is 1.
!
!    * The leading 1 in a given row occurs in a column to
!      the right of the leading 1 in the previous row.
!
!    * Rows which are entirely zero must occur last.
!
!    The matrix is in reduced row echelon form if, in addition to
!    the first three conditions, it also satisfies:
!
!    * Each column containing a leading 1 has no other nonzero entries.
!
!  Example:
!
!    Input matrix:
!
!     1.0  3.0  0.0  2.0  6.0  3.0  1.0
!    -2.0 -6.0  0.0 -2.0 -8.0  3.0  1.0
!     3.0  9.0  0.0  0.0  6.0  6.0  2.0
!    -1.0 -3.0  0.0  1.0  0.0  9.0  3.0
!
!    Output matrix:
!
!     1.0  3.0  0.0  0.0  2.0  0.0  0.0
!     0.0  0.0  0.0  1.0  2.0  0.0  0.0
!     0.0  0.0  0.0  0.0  0.0  1.0  0.3
!     0.0  0.0  0.0  0.0  0.0  0.0  0.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Charles Cullen,
!    An Introduction to Numerical Linear Algebra,
!    PWS Publishing Company, 1994,
!    ISBN: 978-0534936903,
!    LC: QA185.D37.C85.
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix A.
!
!    real ( kind = 8 ) A(M,N).  On the matrix to be analyzed.  
!
!  Output:
!
!    real ( kind = 8 ) A(M,N), the RREF form of the matrix.
!
!    real ( kind = 8 ) DET, the pseudo-determinant.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) lead
  integer ( kind = 4 ) r
  real ( kind = 8 ) r8_epsilon
  real ( kind = 8 ) temp
  real ( kind = 8 ) tol

  det = 1.0D+00
  tol = r8_epsilon ( ) * sum ( abs ( a(1:m,1:n) ) )
  lead = 1

  do r = 1, m

    if ( n < lead ) then
      exit
    end if

    i = r

    do while ( abs ( a(i,lead) ) <= tol )

      i = i + 1

      if ( m < i ) then
        i = r
        lead = lead + 1
        if ( n < lead ) then
          lead = -1
          exit
        end if
      end if

    end do

    if ( lead < 0 ) then
      exit
    end if

    do j = 1, n
      temp   = a(i,j)
      a(i,j) = a(r,j)
      a(r,j) = temp
    end do

    det = det * a(r,lead)
    a(r,1:n) = a(r,1:n) / a(r,lead)

    do i = 1, m
      if ( i /= r ) then
        a(i,1:n) = a(i,1:n) - a(i,lead) * a(r,1:n)
      end if
    end do

    lead = lead + 1

  end do

  return
end
subroutine r8mat_scale ( m, n, s, a )

!*****************************************************************************80
!
!! R8MAT_SCALE multiplies an R8MAT by a scalar.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 December 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) S, the scale factor.
!
!    real ( kind = 8 ) A(M,N), the matrix to be scaled.
!
!  Output:
!
!    real ( kind = 8 ) A(M,N), the scaled matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) s

  a(1:m,1:n) = a(1:m,1:n) * s

  return
end
subroutine r8mat_scale_01 ( m, n, x, xs )

!*****************************************************************************80
!
!! R8MAT_SCALE_01 scales an R8MAT to [0,1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) X(M,N), the array to be scaled.
!
!  Output:
!
!    real ( kind = 8 ) XS(M,N), the scaled array.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  real ( kind = 8 ) x(m,n)
  real ( kind = 8 ) xmax(n)
  real ( kind = 8 ) xmin(n)
  real ( kind = 8 ) xs(m,n)

  xs(1:m,1:n) = x(1:m,1:n)
  call r8mat_max_columns ( m, n, x, xmax )
  call r8mat_min_columns ( m, n, x, xmin )
  do j = 1, n
    if ( 0.0D+00 < xmax(j) - xmin(j) ) then
      xs(1:m,j) = ( xs(1:m,j) - xmin(j) ) / ( xmax(j) - xmin(j) )
    else
      xs(1:m,j) = 0.5D+00
    end if
  end do

  return
end
subroutine r8mat_scale_ab ( m, n, x, a, b, xs )

!*****************************************************************************80
!
!! R8MAT_SCALE_AB scales an R8MAT to [A,B].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) X(M,N), the array to be scaled.
!
!    real ( kind = 8 ) A, B, the new limits.
!
!  Output:
!
!    real ( kind = 8 ) XS(M,N), the scaled array.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(m,n)
  real ( kind = 8 ) xmax(n)
  real ( kind = 8 ) xmin(n)
  real ( kind = 8 ) xs(m,n)

  xs(1:m,1:n) = x(1:m,1:n)
  call r8mat_max_columns ( m, n, x, xmax )
  call r8mat_min_columns ( m, n, x, xmin )
  do j = 1, n
    if ( 0.0D+00 < xmax(j) - xmin(j) ) then
      xs(1:m,j) = a + ( b - a ) * ( xs(1:m,j) - xmin(j) ) / ( xmax(j) - xmin(j) )
    else
      xs(1:m,j) = ( a + b ) / 2.0D+00
    end if
  end do

  return
end
subroutine r8mat_solve ( n, rhs_num, a, info )

!*****************************************************************************80
!
!! R8MAT_SOLVE uses Gauss-Jordan elimination to solve an N by N linear system.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix.
!
!    integer ( kind = 4 ) RHS_NUM, the number of right hand sides.
!    RHS_NUM must be at least 0.
!
!    real ( kind = 8 ) A(N,N+RHS_NUM), contains in rows and
!    columns 1 to N the coefficient matrix, and in columns N+1 through
!    N+RHS_NUM, the right hand sides.  
!
!  Output:
!
!    real ( kind = 8 ) A(N,N+RHS_NUM), the coefficient matrix
!    area has been destroyed, while the right hand sides have
!    been overwritten with the corresponding solutions.
!
!    integer ( kind = 4 ) INFO, singularity flag.
!    0, the matrix was not singular, the solutions were computed;
!    J, factorization failed on step J, and the solutions could not
!    be computed.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) rhs_num

  real ( kind = 8 ) a(n,n+rhs_num)
  real ( kind = 8 ) apivot
  real ( kind = 8 ) factor
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipivot
  integer ( kind = 4 ) j
  real ( kind = 8 ) t(n+rhs_num)

  info = 0

  do j = 1, n
!
!  Choose a pivot row.
!
    ipivot = j
    apivot = a(j,j)

    do i = j + 1, n
      if ( abs ( apivot ) < abs ( a(i,j) ) ) then
        apivot = a(i,j)
        ipivot = i
      end if
    end do

    if ( apivot == 0.0D+00 ) then
      info = j
      return
    end if
!
!  The pivot row moves into the J-th row.
!
    if ( ipivot /= j ) then
      t(       1:n+rhs_num) = a(ipivot,1:n+rhs_num)
      a(ipivot,1:n+rhs_num) = a(j,     1:n+rhs_num)
      a(j,     1:n+rhs_num) = t(       1:n+rhs_num)
    end if
!
!  A(J,J) becomes 1.
!
    a(j,j) = 1.0D+00
    a(j,j+1:n+rhs_num) = a(j,j+1:n+rhs_num) / apivot
!
!  A(I,J) becomes 0.
!
    do i = 1, n

      if ( i /= j ) then
        factor = a(i,j)
        a(i,j) = 0.0D+00
        a(i,j+1:n+rhs_num) = a(i,j+1:n+rhs_num) - factor * a(j,j+1:n+rhs_num)
      end if

    end do

  end do

  return
end
subroutine r8mat_solve_2d ( a, b, det, x )

!*****************************************************************************80
!
!! R8MAT_SOLVE_2D solves a 2 by 2 linear system using Cramer's rule.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If the determinant DET is returned as zero, then the matrix A is
!    singular, and does not have an inverse.  In that case, X is
!    returned as the zero vector.
!
!    If DET is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A(2,2), the matrix.
!
!    real ( kind = 8 ) B(2), the right hand side.
!
!  Output:
!
!    real ( kind = 8 ) DET, the determinant of the matrix A.
!
!    real ( kind = 8 ) X(2), the solution of the system,
!    if DET is nonzero.
!
  implicit none

  real ( kind = 8 ) a(2,2)
  real ( kind = 8 ) b(2)
  real ( kind = 8 ) det
  real ( kind = 8 ) x(2)
!
!  Compute the determinant.
!
  det = a(1,1) * a(2,2) - a(1,2) * a(2,1)
!
!  If the determinant is zero, bail out.
!
  if ( det == 0.0D+00 ) then
    x(1:2) = 0.0D+00
    return
  end if
!
!  Compute the solution.
!
  x(1) = (  a(2,2) * b(1) - a(1,2) * b(2) ) / det
  x(2) = ( -a(2,1) * b(1) + a(1,1) * b(2) ) / det

  return
end
subroutine r8mat_solve_3d ( a, b, det, x )

!*****************************************************************************80
!
!! R8MAT_SOLVE_3D solves a 3 by 3 linear system using Cramer's rule.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    If the determinant DET is returned as zero, then the matrix A is
!    singular, and does not have an inverse.  In that case, X is
!    returned as the zero vector.
!
!    If DET is nonzero, then its value is roughly an estimate
!    of how nonsingular the matrix A is.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2006
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A(3,3), the matrix.
!
!    real ( kind = 8 ) B(3), the right hand side.
!
!  Output:
!
!    real ( kind = 8 ) DET, the determinant of the matrix A.
!
!    real ( kind = 8 ) X(3), the solution of the system,
!    if DET is nonzero.
!
  implicit none

  real ( kind = 8 ) a(3,3)
  real ( kind = 8 ) b(3)
  real ( kind = 8 ) det
  real ( kind = 8 ) x(3)
!
!  Compute the determinant.
!
  det =  a(1,1) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) &
       + a(1,2) * ( a(2,3) * a(3,1) - a(2,1) * a(3,3) ) &
       + a(1,3) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) )
!
!  If the determinant is zero, bail out.
!
  if ( det == 0.0D+00 ) then
    x(1:3) = 0.0D+00
    return
  end if
!
!  Compute the solution.
!
  x(1) = (   ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) * b(1) &
           - ( a(1,2) * a(3,3) - a(1,3) * a(3,2) ) * b(2) &
           + ( a(1,2) * a(2,3) - a(1,3) * a(2,2) ) * b(3) ) / det

  x(2) = ( - ( a(2,1) * a(3,3) - a(2,3) * a(3,1) ) * b(1) &
           + ( a(1,1) * a(3,3) - a(1,3) * a(3,1) ) * b(2) &
           - ( a(1,1) * a(2,3) - a(1,3) * a(2,1) ) * b(3) ) / det

  x(3) = (   ( a(2,1) * a(3,2) - a(2,2) * a(3,1) ) * b(1) &
           - ( a(1,1) * a(3,2) - a(1,2) * a(3,1) ) * b(2) &
           + ( a(1,1) * a(2,2) - a(1,2) * a(2,1) ) * b(3) ) / det

  return
end
subroutine r8mat_solve2 ( n, a, b, x, ierror )

!*****************************************************************************80
!
!! R8MAT_SOLVE2 computes the solution of an N by N linear system.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The linear system may be represented as
!
!      A*X = B
!
!    If the linear system is singular, but consistent, then the routine will
!    still produce a solution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of equations.
!
!    real ( kind = 8 ) A(N,N), the coefficient matrix to be inverted.
!
!    real ( kind = 8 ) B(N), the right hand side of the system.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), A has been overwritten.
!
!    real ( kind = 8 ) B(N), B has been overwritten.
!
!    real ( kind = 8 ) X(N), the solution of the linear system.
!
!    integer ( kind = 4 ) IERROR.
!    0, no error detected.
!    1, consistent singularity.
!    2, inconsistent singularity.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) imax
  integer ( kind = 4 ) ipiv(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(n)

  ierror = 0

  ipiv(1:n) = 0
  x(1:n) = 0.0D+00
!
!  Process the matrix.
!
  do k = 1, n
!
!  In column K:
!    Seek the row IMAX with the properties that:
!      IMAX has not already been used as a pivot;
!      A(IMAX,K) is larger in magnitude than any other candidate.
!
    amax = 0.0D+00
    imax = 0
    do i = 1, n
      if ( ipiv(i) == 0 ) then
        if ( amax < abs ( a(i,k) ) ) then
          imax = i
          amax = abs ( a(i,k) )
        end if
      end if
    end do
!
!  If you found a pivot row IMAX, then,
!    eliminate the K-th entry in all rows that have not been used for pivoting.
!
    if ( imax /= 0 ) then

      ipiv(imax) = k
      a(imax,k+1:n) = a(imax,k+1:n) / a(imax,k)
      b(imax) = b(imax) / a(imax,k)
      a(imax,k) = 1.0D+00

      do i = 1, n

        if ( ipiv(i) == 0 ) then
          a(i,k+1:n) = a(i,k+1:n) - a(i,k) * a(imax,k+1:n)
          b(i) = b(i) - a(i,k) * b(imax)
          a(i,k) = 0.0D+00
        end if

      end do

    end if

  end do
!
!  Now, every row with nonzero IPIV begins with a 1, and
!  all other rows are all zero.  Begin solution.
!
  do j = n, 1, -1

    imax = 0
    do k = 1, n
      if ( ipiv(k) == j ) then
        imax = k
      end if
    end do

    if ( imax == 0 ) then

      x(j) = 0.0D+00

      if ( b(j) == 0.0D+00 ) then
        ierror = 1
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_SOLVE2 - Warning:'
        write ( *, '(a,i8)' ) '  Consistent singularity, equation = ', j
      else
        ierror = 2
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_SOLVE2 - Error:'
        write ( *, '(a,i8)' ) '  Inconsistent singularity, equation = ', j
      end if

    else

      x(j) = b(imax)

      do i = 1, n
        if ( i /= imax ) then
          b(i) = b(i) - a(i,j) * x(j)
        end if
      end do

    end if

  end do

  return
end
subroutine r8mat_standardize ( m, n, x, xs )

!*****************************************************************************80
!
!! R8MAT_STANDARDIZE standardizes an R8MAT.
!
!  Discussion:
!
!    The output array will have columns of 0 mean and unit standard deviation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) X(M,N), the array to be standardized.
!
!  Output:
!
!    real ( kind = 8 ) XS(M,N), the standardized array.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  real ( kind = 8 ) mu(n)
  real ( kind = 8 ) sigma(n)
  real ( kind = 8 ) x(m,n)
  real ( kind = 8 ) xs(m,n)

  call r8mat_mean_columns ( m, n, x, mu )
  call r8mat_std_columns ( m, n, x, sigma )
  
  do j = 1, n
    if ( sigma(j) /= 0.0D+00 ) then
      xs(1:m,j) = ( x(1:m,j) - mu(j) ) / sigma(j)
    else
      xs(1:m,j) = 0.0D+00
    end if
  end do

  return
end
subroutine r8mat_std_columns ( m, n, a, stds )

!*****************************************************************************80
!
!! R8MAT_STD_COLUMNS returns the column standard deviations of an M by N R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_STD_COLUMNS(N), the column stds.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) stds(n)

  do j = 1, n
    call r8vec_std_sample ( m, a(1:m,j), stds(j) )
  end do

  return
end
subroutine r8mat_std_rows ( m, n, a, stds )

!*****************************************************************************80
!
!! R8MAT_STD_ROWS returns the row standard deviations of an M by N R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(M,N), the matrix.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_STD_ROWS(M), the row stds.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) stds(m)

  do i = 1, m
    call r8vec_std_sample ( n, a(i,1:n), stds(i) )
  end do

  return
end
function r8mat_sum ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_SUM returns the sum of the entries of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    In FORTRAN90, this facility is offered by the built in
!    SUM function:
!
!      R8MAT_SUM ( M, N, A ) = SUM ( A(1:M,1:N) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_SUM, the sum of the entries.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_sum

  r8mat_sum = sum ( a(1:m,1:n) )

  return
end
subroutine r8mat_symm_eigen ( n, x, q, a )

!*****************************************************************************80
!
!! R8MAT_SYMM_EIGEN returns a symmetric matrix with given eigensystem.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The user must supply the desired eigenvalue vector, and the desired
!    eigenvector matrix.  The eigenvector matrix must be orthogonal.  A
!    suitable random orthogonal matrix can be generated by R8MAT_ORTH_UNIFORM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of A.
!
!    real ( kind = 8 ) X(N), the desired eigenvalues for the matrix.
!
!    real ( kind = 8 ) Q(N,N), the eigenvector matrix of A.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), a symmetric matrix with
!    eigenvalues X and eigenvectors the columns of Q.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) q(n,n)
  real ( kind = 8 ) x(n)
!
!  Set A = Q * Lambda * Q'.
!
  a(1:n,1:n) = 0.0D+00

  do i = 1, n
    do j = 1, n
      do k = 1, n
        a(i,j) = a(i,j) + q(i,k) * x(k) * q(j,k)
      end do
    end do
  end do

  return
end
subroutine r8mat_symm_jacobi ( n, a )

!*****************************************************************************80
!
!! R8MAT_SYMM_JACOBI applies Jacobi eigenvalue iteration to a symmetric matrix.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    This code was modified so that it treats as zero the off-diagonal
!    elements that are sufficiently close to, but not exactly, zero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of A.
!
!    real ( kind = 8 ) A(N,N), a symmetric N by N matrix.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), the matrix has been overwritten by an
!    approximately diagonal matrix, with the eigenvalues on the diagonal.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) c
  real ( kind = 8 ) r8mat_norm_fro
  real ( kind = 8 ), parameter :: eps = 0.00001D+00
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it
  integer ( kind = 4 ), parameter :: it_max = 100
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) norm_fro
  real ( kind = 8 ) s
  real ( kind = 8 ) sum2
  real ( kind = 8 ) t
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) u

  norm_fro = r8mat_norm_fro ( n, n, a )

  it = 0

  do

    it = it + 1

    do i = 1, n
      do j = 1, i - 1

        if ( eps * norm_fro < abs ( a(i,j) ) + abs ( a(j,i) ) ) then

          u = ( a(j,j) - a(i,i) ) / ( a(i,j) + a(j,i) )

          t = sign ( 1.0D+00, u ) / ( abs ( u ) + sqrt ( u * u + 1.0D+00 ) )
          c = 1.0D+00 / sqrt ( t * t + 1.0D+00 )
          s = t * c
!
!  A -> A * Q.
!
          do k = 1, n
            t1 = a(i,k)
            t2 = a(j,k)
            a(i,k) = t1 * c - t2 * s
            a(j,k) = t1 * s + t2 * c
          end do
!
!  A -> QT * A
!
          do k = 1, n
            t1 = a(k,i)
            t2 = a(k,j)
            a(k,i) = c * t1 - s * t2
            a(k,j) = s * t1 + c * t2
          end do

        end if
      end do
    end do
!
!  Test the size of the off-diagonal elements.
!
    sum2 = 0.0D+00
    do i = 1, n
      do j = 1, i - 1
        sum2 = sum2 + abs ( a(i,j) )
      end do
    end do

    if ( sum2 <= eps * ( norm_fro + 1.0D+00 ) ) then
      exit
    end if

    if ( it_max <= it ) then
      exit
    end if

  end do

  return
end
subroutine r8mat_to_r8cmat ( lda, m, n, a1, a2 )

!*****************************************************************************80
!
!! R8MAT_TO_R8CMAT transfers data from an R8MAT to an R8CMAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, 
!    accessible as a vector:
!      (I,J) -> (I+J*M).
!    or as a doubly-dimensioned array, if declared A(M,N):
!      (I,J) -> A(I,J)      
!
!    An R8CMAT is an MxN array of R8's, stored with a leading dimension LD,
!    accessible as a vector:
!      (I,J) -> (I+J*LD).
!    or as a doubly-dimensioned array, if declared A(LD,N):
!      (I,J) -> A(I,J)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 March 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) LDA, the leading dimension of A2.
!
!    integer ( kind = 4 ) M, the number of rows of data.
!    M <= LDA.
!
!    integer ( kind = 4 ) N, the number of columns of data.
!
!    real ( kind = 8 ) A1(M,N), the matrix to be copied.
!
!  Output:
!
!    real ( kind = 8 ) A2(LDA,N), contains a copy of the
!    information in A1, in the MxN submatrix.
!
  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(m,n)
  real ( kind = 8 ) a2(lda,n)

  a2(1:m,1:n) = a1(1:m,1:n)
 
  return
end
subroutine r8mat_to_r8plu ( n, a, pivot, lu, info )

!*****************************************************************************80
!
!! R8MAT_TO_R8PLU factors a general R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    This routine is a simplified version of the LINPACK routine DGEFA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1.
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    real ( kind = 8 ) A(N,N), the matrix to be factored.
!
!  Output:
!
!    integer ( kind = 4 ) PIVOT(N), a vector of pivot indices.
!
!    real ( kind = 8 ) LU(N,N), an upper triangular matrix U and
!    the multipliers L which were used to obtain it.  The factorization
!    can be written A = L * U, where L is a product of permutation and
!    unit lower triangular matrices and U is upper triangular.
!
!    integer ( kind = 4 ) INFO, singularity flag.
!    0, no singularity detected.
!    nonzero, the factorization failed on the INFO-th step.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) info
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) lu(n,n)
  real ( kind = 8 ) temp

  lu(1:n,1:n) = a(1:n,1:n)

  info = 0

  do k = 1, n - 1
!
!  Find L, the index of the pivot row.
!
    l = k
    do i = k + 1, n
      if ( abs ( lu(l,k) ) < abs ( lu(i,k) ) ) then
        l = i
      end if
    end do

    pivot(k) = l
!
!  If the pivot index is zero, the algorithm has failed.
!
    if ( lu(l,k) == 0.0D+00 ) then
      info = k
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_TO_R8PLU - Fatal error!'
      write ( *, '(a,i8)' ) '  Zero pivot on step ', info
      return
    end if
!
!  Interchange rows L and K if necessary.
!
    if ( l /= k ) then
      temp    = lu(l,k)
      lu(l,k) = lu(k,k)
      lu(k,k) = temp
    end if
!
!  Normalize the values that lie below the pivot entry A(K,K).
!
    lu(k+1:n,k) = -lu(k+1:n,k) / lu(k,k)
!
!  Row elimination with column indexing.
!
    do j = k + 1, n

      if ( l /= k ) then
        temp    = lu(l,j)
        lu(l,j) = lu(k,j)
        lu(k,j) = temp
      end if

      lu(k+1:n,j) = lu(k+1:n,j) + lu(k+1:n,k) * lu(k,j)

    end do

  end do

  pivot(n) = n

  if ( lu(n,n) == 0.0D+00 ) then
    info = n
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_TO_R8PLU - Fatal error!'
    write ( *, '(a,i8)' ) '  Zero pivot on step ', info
  end if

  return
end
function r8mat_trace ( n, a )

!*****************************************************************************80
!
!! R8MAT_TRACE computes the trace of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The trace of a square matrix is the sum of the diagonal elements.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix A.
!
!    real ( kind = 8 ) A(N,N), the matrix whose trace is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_TRACE, the trace of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8mat_trace

  r8mat_trace = 0.0D+00
  do i = 1, n
    r8mat_trace = r8mat_trace + a(i,i)
  end do

  return
end
subroutine r8mat_transpose ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE transposes an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an M by N array of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) A(M,N), an M by N matrix.
!
!  Output:
!
!    real ( kind = 8 ) A(N,M), the transposed matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m*n)
  real ( kind = 8 ), allocatable :: b(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  allocate ( b(m*n) )

  do j = 1, n
    do i = 1, m
      b(j+(i-1)*n)= a(i+(j-1)*m)
    end do
  end do

  do j = 1, n
    do i = 1, m
      a(j+(i-1)*n) = b(j+(i-1)*n)
    end do
  end do
  
  deallocate ( b )

  return
end
subroutine r8mat_transpose_in_place ( n, a )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_IN_PLACE transposes an R8MAT in place.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2008
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns
!    of the matrix A.
!
!    real ( kind = 8 ) A(N,N), the matrix to be transposed.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), the transposed matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) t

  do j = 1, n
    do i = 1, j - 1
      t      = a(i,j)
      a(i,j) = a(j,i)
      a(j,i) = t
    end do
  end do

  return
end
subroutine r8mat_transpose_new ( m, n, a, at )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_NEW makes a transposed copy of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    FORTRAN90 provides the transpose ( ) function which should be preferred
!    over this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns
!    of the matrix A.
!
!    real ( kind = 8 ) A(N,N), the matrix to be transposed.
!
!  Output:
!
!    real ( kind = 8 ) AT(N,M), the transposed matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) at(n,m)

  at = transpose ( a )

  return
end
subroutine r8mat_transpose_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = * ) title

  call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

  return
end
subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

!*****************************************************************************80
!
!! R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT, transposed.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ), parameter :: incx = 5
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 14 ) ctemp(incx)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i2hi
  integer ( kind = 4 ) i2lo
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2hi
  integer ( kind = 4 ) j2lo
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  if ( m <= 0 .or. n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  (None)'
    return
  end if

  do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

    i2hi = i2lo + incx - 1
    i2hi = min ( i2hi, m )
    i2hi = min ( i2hi, ihi )

    inc = i2hi + 1 - i2lo

    write ( *, '(a)' ) ' '

    do i = i2lo, i2hi
      i2 = i + 1 - i2lo
      write ( ctemp(i2), '(i8,6x)' ) i
    end do

    write ( *, '(''  Row   '',5a14)' ) ctemp(1:inc)
    write ( *, '(a)' ) '  Col'
    write ( *, '(a)' ) ' '

    j2lo = max ( jlo, 1 )
    j2hi = min ( jhi, n )

    do j = j2lo, j2hi

      do i2 = 1, inc
        i = i2lo - 1 + i2
        write ( ctemp(i2), '(g14.6)' ) a(i,j)
      end do

      write ( *, '(i5,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

    end do

  end do

  return
end
subroutine r8mat_u_inverse ( n, a, b )

!*****************************************************************************80
!
!! R8MAT_U_INVERSE inverts an upper triangular R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    An upper triangular matrix is a matrix whose only nonzero entries
!    occur on or above the diagonal.
!
!    The inverse of an upper triangular matrix is an upper triangular matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, number of rows and columns in the matrix.
!
!    real ( kind = 8 ) A(N,N), the upper triangular matrix.
!
!  Output:
!
!    real ( kind = 8 ) B(N,N), the inverse matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do j = n, 1, -1

    do i = n, 1, -1

      if ( j < i ) then
        b(i,j) = 0.0D+00
      else if ( i == j ) then
        b(i,j) = 1.0D+00 / a(i,j)
      else
        b(i,j) = - dot_product ( a(i,i+1:j), b(i+1:j,j) ) / a(i,i)
      end if

    end do
  end do

  return
end
subroutine r8mat_u_solve ( n, a, b, x )

!*****************************************************************************80
!
!! R8MAT_U_SOLVE solves an upper triangular linear system.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns of
!    the matrix A.
!
!    real ( kind = 8 ) A(N,N), the N by N upper triangular matrix.
!
!    real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
!
!  Solve U * x = b.
!
  do i = n, 1, -1
    x(i) = ( b(i) - dot_product ( a(i,i+1:n), x(i+1:n) ) ) / a(i,i)
  end do

  return
end
subroutine r8mat_u1_inverse ( n, a, b )

!*****************************************************************************80
!
!! R8MAT_U1_INVERSE inverts a unit upper triangular R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A unit upper triangular matrix is a matrix with only 1's on the main
!    diagonal, and only 0's below the main diagonal.
!
!    The inverse of a unit upper triangular matrix is also
!    a unit upper triangular matrix.
!
!    This routine can invert a matrix in place, that is, with no extra
!    storage.  If the matrix is stored in A, then the call
!
!      call r8mat_u1_inverse ( n, a, a )
!
!    will result in A being overwritten by its inverse.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 December 2004
!
!  Author:
!
!    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
!    FORTRAN90 version by John Burkardt,
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, number of rows and columns in the matrix.
!
!    real ( kind = 8 ) A(N,N), the unit upper triangular matrix.
!
!  Output:
!
!    real ( kind = 8 ) B(N,N), the inverse matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do j = n, 1, -1

    do i = n, 1, -1

      if ( j < i ) then
        b(i,j) = 0.0D+00
      else if ( i == j ) then
        b(i,j) = 1.0D+00
      else
        b(i,j) = -dot_product ( a(i,i+1:j), b(i+1:j,j) )
      end if

    end do
  end do

  return
end
subroutine r8mat_uniform_01 ( m, n, seed, r )

!*****************************************************************************80
!
!! r8mat_uniform_01 fills an R8MAT with unit pseudorandom numbers.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns in
!    the array.
!
!    integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0. 
!
!  Input:
!
!    real ( kind = 8 ) R(M,N), the array of pseudorandom values.
!
!    integer ( kind = 4 ) SEED, the updated seed value.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if

      r(i,j) = real ( seed, kind = 8 ) * 4.656612875D-10

    end do
  end do

  return
end
subroutine r8mat_uniform_ab ( m, n, a, b, seed, r )

!*****************************************************************************80
!
!! R8MAT_UNIFORM_AB returns a scaled pseudorandom R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A <= R(I,J) <= B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    real ( kind = 8 ) A, B, the lower and upper limits.
!
!    integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.
!
!  Output:
!
!    real ( kind = 8 ) R(M,N), the array of pseudorandom values.
!
!    integer ( kind = 4 ) SEED, an updated seed value.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if

      r(i,j) = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

    end do
  end do

  return
end
subroutine r8mat_uniform_abvec ( m, n, a, b, seed, r )

!*****************************************************************************80
!
!! R8MAT_UNIFORM_ABVEC returns a scaled pseudorandom R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    A(I) <= R(I,J) <= B(I)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    real ( kind = 8 ) A(M), B(M), the lower and upper limits.
!
!    integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.
!
!  Output:
!
!    real ( kind = 8 ) R(M,N), the array of pseudorandom values.
!
!    integer ( kind = 4 ) SEED, an updated seed value.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m)
  real ( kind = 8 ) b(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(m,n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_UNIFORM_ABVEC - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do j = 1, n

    do i = 1, m

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed < 0 ) then
        seed = seed + i4_huge
      end if

      r(i,j) = a(i) + ( b(i) - a(i) ) * real ( seed, kind = 8 ) &
        * 4.656612875D-10

    end do
  end do

  return
end
subroutine r8mat_ut_solve ( n, a, b, x )

!*****************************************************************************80
!
!! R8MAT_UT_SOLVE solves a transposed upper triangular linear system.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    Given the upper triangular matrix A, the linear system to be solved is:
!
!      A' * x = b
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of rows and columns
!    of the matrix.
!
!    real ( kind = 8 ) A(N,N), the N by N upper triangular matrix.
!
!    real ( kind = 8 ) B(N), the right hand side of the linear system.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the solution of the linear system.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
!
!  Solve U' * x = b.
!
  do i = 1, n
    x(i) = ( b(i) - dot_product ( x(1:i-1), a(1:i-1,i) ) ) / a(i,i)
  end do

  return
end
subroutine r8mat_vand2 ( n, x, a )

!*****************************************************************************80
!
!! R8MAT_VAND2 returns the N by N row Vandermonde matrix A.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The row Vandermonde matrix returned by this routine reads "across"
!    rather than down.  In particular, each row begins with a 1, followed by
!    some value X, followed by successive powers of X.
!
!  Formula:
!
!    A(I,J) = X(I)^(J-1)
!
!  Properties:
!
!    A is nonsingular if, and only if, the X values are distinct.
!
!    The determinant of A is
!
!      det(A) = product ( 2 <= I <= N ) (
!        product ( 1 <= J <= I-1 ) ( ( X(I) - X(J) ) ) ).
!
!    The matrix A is generally ill-conditioned.
!
!  Example:
!
!    N = 5, X = (2, 3, 4, 5, 6)
!
!    1 2  4   8   16
!    1 3  9  27   81
!    1 4 16  64  256
!    1 5 25 125  625
!    1 6 36 216 1296
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix desired.
!
!    real ( kind = 8 ) X(N), the values that define A.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), the N by N row Vandermonde matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)

  do i = 1, n
    do j = 1, n

      if ( j == 1 .and. x(i) == 0.0D+00 ) then
        a(i,j) = 1.0D+00
      else
        a(i,j) = x(i) ** ( j - 1 )
      end if

    end do
  end do

  return
end
function r8mat_vtmv ( m, n, x, a, y )

!*****************************************************************************80
!
!! R8MAT_VTMV multiplies computes the scalar x' * A * y.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix.
!
!    real ( kind = 8 ) X(N), the first vector factor.
!
!    real ( kind = 8 ) A(M,N), the M by N matrix.
!
!    real ( kind = 8 ) Y(M), the second vector factor.
!
!  Output:
!
!    real ( kind = 8 ) R8MAT_VTMV, the value of X' * A * Y.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) r8mat_vtmv
  real ( kind = 8 ) x(m)
  real ( kind = 8 ) y(n)

  r8mat_vtmv = dot_product ( x(1:m), matmul ( a(1:m,1:n), y(1:n) ) )

  return
end
subroutine r8mat_zeros ( m, n, a )

!*****************************************************************************80
!
!! R8MAT_ZEROS zeroes an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!  Output:
!
!    real ( kind = 8 ) A(M,N), the matrix of zeroes.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)

  a(1:m,1:n) = 0.0D+00

  return
end
subroutine r8plu_det ( n, pivot, lu, det )

!*****************************************************************************80
!
!! R8PLU_DET computes the determinant of an R8PLU matrix.
!
!  Discussion:
!
!    The matrix should have been factored by R8MAT_TO_R8PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1.
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    integer ( kind = 4 ) PIVOT(N), the pivot vector computed
!    by R8MAT_TO_R8PLU.
!
!    real ( kind = 8 ) LU(N,N), the LU factors computed
!    by R8MAT_TO_R8PLU.
!
!  Output:
!
!    real ( kind = 8 ) DET, the determinant of the matrix.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) det
  integer ( kind = 4 ) i
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)

  det = 1.0D+00

  do i = 1, n
    det = det * lu(i,i)
    if ( pivot(i) /= i ) then
      det = -det
    end if
  end do

  return
end
subroutine r8plu_inverse ( n, pivot, lu, a_inverse )

!*****************************************************************************80
!
!! R8PLU_INVERSE computes the inverse of an R8PLU matrix.
!
!  Discussion:
!
!    The matrix should have been factored by R8MAT_TO_R8PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix A.
!
!    integer ( kind = 4 ) PIVOT(N), the pivot vector from
!    R8MAT_TO_R8PLU.
!
!    real ( kind = 8 ) LU(N,N), the LU factors computed by
!    R8MAT_TO_R8PLU.
!
!  Output:
!
!    real ( kind = 8 ) A_INVERSE(N,N), the inverse of the original
!    matrix A that was factored by R8MAT_TO_R8PLU.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a_inverse(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) temp
  real ( kind = 8 ) work(n)

  a_inverse(1:n,1:n) = lu(1:n,1:n)
!
!  Compute Inverse(U).
!
  do k = 1, n

    a_inverse(k,k)     = 1.0D+00 / a_inverse(k,k)
    a_inverse(1:k-1,k) = -a_inverse(1:k-1,k) * a_inverse(k,k)

    do j = k + 1, n

      temp             = a_inverse(k,j)
      a_inverse(k,j)   = 0.0D+00
      a_inverse(1:k,j) = a_inverse(1:k,j) + temp * a_inverse(1:k,k)

    end do

  end do
!
!  Form Inverse(U) * Inverse(L).
!
  do k = n - 1, 1, -1

    work(k+1:n) = a_inverse(k+1:n,k)
    a_inverse(k+1:n,k) = 0.0D+00

    do j = k + 1, n
      a_inverse(1:n,k) = a_inverse(1:n,k) + a_inverse(1:n,j) * work(j)
    end do

    if ( pivot(k) /= k ) then

      do i = 1, n
        temp                  = a_inverse(i,k)
        a_inverse(i,k)        = a_inverse(i,pivot(k))
        a_inverse(i,pivot(k)) = temp
      end do

    end if

  end do

  return
end
subroutine r8plu_mul ( n, pivot, lu, x, b )

!*****************************************************************************80
!
!! R8PLU_MUL computes A * x using the PLU factors of A.
!
!  Discussion:
!
!    It is assumed that R8MAT_TO_R8PLU has computed the PLU factors of
!    the matrix A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    integer ( kind = 4 ) PIVOT(N), the pivot vector computed
!    by R8MAT_TO_R8PLU.
!
!    real ( kind = 8 ) LU(N,N), the matrix factors computed by
!    R8MAT_TO_R8PLU.
!
!    real ( kind = 8 ) X(N), the vector to be multiplied.
!
!  Output:
!
!    real ( kind = 8 ) B(N), the result of the multiplication.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)
  real ( kind = 8 ) temp
  real ( kind = 8 ) x(n)

  b(1:n) = x(1:n)
!
!  Y = U * X.
!
  do j = 1, n
    b(1:j-1) = b(1:j-1) + lu(1:j-1,j) * b(j)
    b(j) = lu(j,j) * b(j)
  end do
!
!  B = PL * Y = PL * U * X = A * x.
!
  do j = n - 1, 1, -1

    b(j+1:n) = b(j+1:n) - lu(j+1:n,j) * b(j)

    k = pivot(j)

    if ( k /= j ) then
      temp = b(k)
      b(k) = b(j)
      b(j) = temp
    end if

  end do

  return
end
subroutine r8plu_solve ( n, pivot, lu, b, x )

!*****************************************************************************80
!
!! R8PLU_SOLVE solves a linear system A*x=b from the PLU factors.
!
!  Discussion:
!
!    The PLU factors should have been computed by R8MAT_TO_R8PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix.
!
!    integer ( kind = 4 ) PIVOT(N), the pivot vector from R8MAT_TO_R8PLU.
!
!    real ( kind = 8 ) LU(N,N), the LU factors from R8MAT_TO_R8PLU.
!
!    real ( kind = 8 ) B(N), the right hand side vector.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the solution vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) lu(n,n)
  real ( kind = 8 ) temp
  real ( kind = 8 ) x(n)
!
!  Solve PL * Y = B.
!
  x(1:n) = b(1:n)

  do k = 1, n - 1

    j = pivot(k)

    if ( j /= k ) then
      temp = x(j)
      x(j) = x(k)
      x(k) = temp
    end if

    x(k+1:n) = x(k+1:n) + lu(k+1:n,k) * x(k)

  end do
!
!  Solve U * X = Y.
!
  do k = n, 1, -1
    x(k) = x(k) / lu(k,k)
    x(1:k-1) = x(1:k-1) - lu(1:k-1,k) * x(k)
  end do

  return
end
subroutine r8plu_to_r8mat ( n, pivot, lu, a )

!*****************************************************************************80
!
!! R8PLU_TO_R8MAT recovers the matrix A that was factored by R8MAT_TO_R8PLU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix.
!    N must be positive.
!
!    integer ( kind = 4 ) PIVOT(N), the pivot vector computed
!    by R8MAT_TO_R8PLU.
!
!    real ( kind = 8 ) LU(N,N), the matrix factors computed by
!    R8MAT_TO_R8PLU.
!
!  Output:
!
!    real ( kind = 8 ) A(N,N), the matrix whose factors are
!    represented by LU and PIVOT.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) lu(n,n)
  integer ( kind = 4 ) pivot(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) temp

  a(1:n,1:n) = 0.0D+00
  do i = 1, n
    a(i,i) = 1.0D+00
  end do

  do j = 1, n

    do i = 1, n
      a(1:i-1,j) = a(1:i-1,j) + lu(1:i-1,i) * a(i,j)
      a(i,j) = lu(i,i) * a(i,j)
    end do
!
!  B = PL * Y = PL * U * X = A * x.
!
    do i = n - 1, 1, -1

      a(i+1:n,j) = a(i+1:n,j) - lu(i+1:n,i) * a(i,j)

      k = pivot(i)

      if ( k /= i ) then
        temp   = a(k,j)
        a(k,j) = a(i,j)
        a(i,j) = temp
      end if

    end do

  end do

  return
end
function r8r8_compare ( x1, y1, x2, y2 )

!*****************************************************************************80
!
!! R8R8_COMPARE compares two R8R8's.
!
!  Discussion:
!
!    An R8R8 is simply a pair of R8 values, stored separately.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X1, Y1, the first vector.
!
!    real ( kind = 8 ) X2, Y2, the second vector.
!
!  Output:
!
!    integer ( kind = 4 ) R8R8_COMPARE:
!    -1, (X1,Y1) < (X2,Y2);
!     0, (X1,Y1) = (X2,Y2);
!    +1, (X1,Y1) > (X2,Y2).
!
  implicit none

  integer ( kind = 4 ) compare
  integer ( kind = 4 ) r8r8_compare
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2

  if ( x1 < x2 ) then
    compare = -1
  else if ( x2 < x1 ) then
    compare = +1
  else if ( y1 < y2 ) then
    compare = -1
  else if ( y2 < y1 ) then
    compare = +1
  else
    compare = 0
  end if

  r8r8_compare = compare

  return
end
subroutine r8r8_print ( a1, a2, title )

!*****************************************************************************80
!
!! R8R8_PRINT prints an R8R8.
!
!  Discussion:
!
!    An R8R8 is simply a pair of R8R8's, stored separately.
!
!    A format is used which suggests a coordinate pair:
!
!  Example:
!
!    Center : ( 1.23, 7.45 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) A1, A2, the coordinates of the vector.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  real ( kind = 8 ) a1
  real ( kind = 8 ) a2
  character ( len = * ) title

  if ( 0 < len_trim ( title ) ) then
    write ( *, '( 2x, a, a4, g14.6, a1, g14.6, a1 )' ) &
      trim ( title ), ' : (', a1, ',', a2, ')'
  else
    write ( *, '( 2x, a1, g14.6, a1, g14.6, a1 )' ) '(', a1, ',', a2, ')'
  end if

  return
end
function r8r8r8_compare ( x1, y1, z1, x2, y2, z2 )

!*****************************************************************************80
!
!! R8R8R8_COMPARE compares two R8R8R8's.
!
!  Discussion:
!
!    An R8R8R8 is simply 3 R8 values, stored as scalars.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X1, Y1, Z1, the first vector.
!
!    real ( kind = 8 ) X2, Y2, Z2, the second vector.
!
!  Output:
!
!    integer ( kind = 4 ) R8R8R8_COMPARE:
!    -1, (X1,Y1,Z1) < (X2,Y2,Z2);
!     0, (X1,Y1,Z1) = (X2,Y2,Z2);
!    +1, (X1,Y1,Z1) > (X2,Y2,Z2).
!
  implicit none

  integer ( kind = 4 ) compare
  integer ( kind = 4 ) r8r8r8_compare
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) z1
  real ( kind = 8 ) z2

  if ( x1 < x2 ) then
    compare = -1
  else if ( x2 < x1 ) then
    compare = +1
  else if ( y1 < y2 ) then
    compare = -1
  else if ( y2 < y1 ) then
    compare = +1
  else if ( z1 < z2 ) then
    compare = -1
  else if ( z2 < z1 ) then
    compare = +1
  else
    compare = 0
  end if

  r8r8r8_compare = compare

  return
end
subroutine r8r8r8vec_index_insert_unique ( n_max, n, x, y, z, indx, &
  xval, yval, zval, ival, ierror )

!*****************************************************************************80
!
!! R8R8R8VEC_INDEX_INSERT_UNIQUE inserts unique R8R8R in an indexed sorted list.
!
!  Discussion:
!
!    An R8R8R8VEC is set of N R8R8R8 items.
!
!    An R8R8R8 is simply 3 R8 values, stored as scalars.
!
!    If the input value does not occur in the current list, it is added,
!    and N, X, Y, Z and INDX are updated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N_MAX, the maximum size of the list.
!
!    integer ( kind = 4 ) N, the size of the list.
!
!    real ( kind = 8 ) X(N), Y(N), Z(N), the R8R8R8 vector.
!
!    integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    real ( kind = 8 ) XVAL, YVAL, ZVAL, the value to be inserted
!    if it is not already in the list.
!
!  Output:
!
!    integer ( kind = 4 ) N, the updated size of the list.
!
!    real ( kind = 8 ) X(N), Y(N), Z(N), the updated R8R8R8 vector.
!
!    integer ( kind = 4 ) INDX(N), the updated sort index of the list.
!
!    integer ( kind = 4 ) IVAL, the index in X, Y, Z corresponding
!    to the value XVAL, YVAL, ZVAL.
!
!    integer ( kind = 4 ) IERROR, 0 for no error, 1 if an error
!    occurred.
!
  implicit none

  integer ( kind = 4 ) n_max

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  integer ( kind = 4 ) n
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(n_max)
  real ( kind = 8 ) yval
  real ( kind = 8 ) z(n_max)
  real ( kind = 8 ) zval

  ierror = 0

  if ( n <= 0 ) then

    if ( n_max <= 0 ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8R8R8VEC_INDEX_INSERT_UNIQUE - Fatal error!'
      write ( *, '(a)' ) '  Not enough space to store new data.'
      return
    end if

    n = 1
    x(1) = xval
    y(1) = yval
    z(1) = zval
    indx(1) = 1
    ival = 1
    return

  end if
!
!  Does ( XVAL, YVAL, ZVAL ) already occur in ( X, Y, Z)?
!
  call r8r8r8vec_index_search ( n, x, y, z, indx, xval, yval, zval, &
    less, equal, more )

  if ( equal == 0 ) then

    if ( n_max <= n ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8R8R8VEC_INDEX_INSERT_UNIQUE - Fatal error!'
      write ( *, '(a)' ) '  Not enough space to store new data.'
      return
    end if

    x(n+1) = xval
    y(n+1) = yval
    z(n+1) = zval
    ival = n + 1
    indx(n+1:more+1:-1) = indx(n:more:-1)
    indx(more) = n + 1
    n = n + 1

  else

    ival = indx(equal)

  end if

  return
end
subroutine r8r8r8vec_index_search ( n, x, y, z, indx, xval, yval, &
  zval, less, equal, more )

!*****************************************************************************80
!
!! R8R8R8VEC_INDEX_SEARCH searches for R8R8R8 value in an indexed sorted list.
!
!  Discussion:
!
!    An R8R8R8VEC is set of N R8R8R8 items.
!
!    An R8R8R8 is simply 3 R8 values, stored as scalars.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the list.
!
!    real ( kind = 8 ) X(N), Y(N), Z(N), the list.
!
!    integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    real ( kind = 8 ) XVAL, YVAL, ZVAL, the value to be sought.
!
!  Output:
!
!    integer ( kind = 4 ) LESS, EQUAL, MORE, the indexes in INDX of the
!    entries of X that are just less than, equal to, and just greater
!    than XVAL.  If XVAL does not occur in X, then EQUAL is zero.
!    If XVAL is the minimum entry of X, then LESS is 0.  If XVAL
!    is the greatest entry of X, then MORE is N+1.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) compare
  integer ( kind = 4 ) r8r8r8_compare
  integer ( kind = 4 ) equal
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) mid
  integer ( kind = 4 ) more
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xmid
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) yhi
  real ( kind = 8 ) ylo
  real ( kind = 8 ) ymid
  real ( kind = 8 ) yval
  real ( kind = 8 ) z(n)
  real ( kind = 8 ) zhi
  real ( kind = 8 ) zlo
  real ( kind = 8 ) zmid
  real ( kind = 8 ) zval

  if ( n <= 0 ) then
    less = 0
    equal = 0
    more = 0
    return
  end if

  lo = 1
  hi = n

  xlo = x(indx(lo))
  ylo = y(indx(lo))
  zlo = z(indx(lo))

  xhi = x(indx(hi))
  yhi = y(indx(hi))
  zhi = z(indx(hi))

  compare = r8r8r8_compare ( xval, yval, zval, xlo, ylo, zlo )

  if ( compare == -1 ) then
    less = 0
    equal = 0
    more = 1
    return
  else if ( compare == 0 ) then
    less = 0
    equal = 1
    more = 2
    return
  end if

  compare = r8r8r8_compare ( xval, yval, zval, xhi, yhi, zhi )

  if ( compare == 1 ) then
    less = n
    equal = 0
    more = n + 1
    return
  else if ( compare == 0 ) then
    less = n - 1
    equal = n
    more = n + 1
    return
  end if

  do

    if ( lo + 1 == hi ) then
      less = lo
      equal = 0
      more = hi
      return
    end if

    mid = ( lo + hi ) / 2
    xmid = x(indx(mid))
    ymid = y(indx(mid))
    zmid = z(indx(mid))

    compare = r8r8r8_compare ( xval, yval, zval, xmid, ymid, zmid )

    if ( compare == 0 ) then
      equal = mid
      less = mid - 1
      more = mid + 1
      return
    else if ( compare == -1 ) then
      hi = mid
    else if ( compare == +1 ) then
      lo = mid
    end if

  end do

  return
end
subroutine r8r8vec_index_insert_unique ( n_max, n, x, y, indx, xval, yval, &
  ival, ierror )

!*****************************************************************************80
!
!! R8R8VEC_INDEX_INSERT_UNIQUE inserts a unique R8R8 in an indexed sorted list.
!
!  Discussion:
!
!    An R8R8VEC is set of N R8R8 items.
!
!    An R8R8 is simply 2 R8 values, stored as scalars.
!
!    If the input value does not occur in the current list, it is added,
!    and N, X, Y and INDX are updated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N_MAX, the maximum size of the list.
!
!    integer ( kind = 4 ) N, the size of the list.
!
!    real ( kind = 8 ) X(N), Y(N), the list of R8R8 vectors.
!
!    integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    real ( kind = 8 ) XVAL, YVAL, the value to be inserted if it is
!    not already in the list.
!
!  Output:
!
!    integer ( kind = 4 ) N, the updated size of the list.
!
!    real ( kind = 8 ) X(N), Y(N), the updated list of R8R8 vectors.
!
!    integer ( kind = 4 ) INDX(N), the updated sort index of the list.
!
!    integer ( kind = 4 ) IVAL, the index in X, Y corresponding to the
!    value XVAL, YVAL.
!
!    integer ( kind = 4 ) IERROR, 0 for no error, 1 if an
!    error occurred.
!
  implicit none

  integer ( kind = 4 ) n_max

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) indx(n_max)
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  integer ( kind = 4 ) n
  real ( kind = 8 ) x(n_max)
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(n_max)
  real ( kind = 8 ) yval

  ierror = 0

  if ( n <= 0 ) then

    if ( n_max <= 0 ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8R8VEC_INDEX_INSERT_UNIQUE - Fatal error!'
      write ( *, '(a)' ) '  Not enough space to store new data.'
      return
    end if

    n = 1
    x(1) = xval
    y(1) = yval
    indx(1) = 1
    ival = 1
    return

  end if
!
!  Does ( XVAL, YVAL ) already occur in ( X, Y )?
!
  call r8r8vec_index_search ( n, x, y, indx, xval, yval, less, equal, more )

  if ( equal == 0 ) then

    if ( n_max <= n ) then
      ierror = 1
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8R8VEC_INDEX_INSERT_UNIQUE - Fatal error!'
      write ( *, '(a)' ) '  Not enough space to store new data.'
      return
    end if

    x(n+1) = xval
    y(n+1) = yval
    ival = n + 1
    indx(n+1:more+1:-1) = indx(n:more:-1)
    indx(more) = n + 1
    n = n + 1

  else

    ival = indx(equal)

  end if

  return
end
subroutine r8r8vec_index_search ( n, x, y, indx, xval, yval, less, equal, &
  more )

!*****************************************************************************80
!
!! R8R8VEC_INDEX_SEARCH searches for an R8R8 in an indexed sorted list.
!
!  Discussion:
!
!    An R8R8VEC is set of N R8R8 items.
!
!    An R8R8 is simply 2 R8 values, stored as scalars.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the current list.
!
!    real ( kind = 8 ) X(N), Y(N), the list.
!
!    integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    real ( kind = 8 ) XVAL, YVAL, the value to be sought.
!
!  Output:
!
!    integer ( kind = 4 ) LESS, EQUAL, MORE, the indexes in INDX of the
!    entries of X that are just less than, equal to, and just greater
!    than XVAL.  If XVAL does not occur in X, then EQUAL is zero.
!    If XVAL is the minimum entry of X, then LESS is 0.  If XVAL
!    is the greatest entry of X, then MORE is N+1.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) compare
  integer ( kind = 4 ) r8r8_compare
  integer ( kind = 4 ) equal
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) mid
  integer ( kind = 4 ) more
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xmid
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) yhi
  real ( kind = 8 ) ylo
  real ( kind = 8 ) ymid
  real ( kind = 8 ) yval

  if ( n <= 0 ) then
    less = 0
    equal = 0
    more = 0
    return
  end if

  lo = 1
  hi = n

  xlo = x(indx(lo))
  ylo = y(indx(lo))

  xhi = x(indx(hi))
  yhi = y(indx(hi))

  compare = r8r8_compare ( xval, yval, xlo, ylo )

  if ( compare == -1 ) then
    less = 0
    equal = 0
    more = 1
    return
  else if ( compare == 0 ) then
    less = 0
    equal = 1
    more = 2
    return
  end if

  compare = r8r8_compare ( xval, yval, xhi, yhi )

  if ( compare == 1 ) then
    less = n
    equal = 0
    more = n + 1
    return
  else if ( compare == 0 ) then
    less = n - 1
    equal = n
    more = n + 1
    return
  end if

  do

    if ( lo + 1 == hi ) then
      less = lo
      equal = 0
      more = hi
      return
    end if

    mid = ( lo + hi ) / 2
    xmid = x(indx(mid))
    ymid = y(indx(mid))

    compare = r8r8_compare ( xval, yval, xmid, ymid )

    if ( compare == 0 ) then
      equal = mid
      less = mid - 1
      more = mid + 1
      return
    else if ( compare == -1 ) then
      hi = mid
    else if ( compare == +1 ) then
      lo = mid
    end if

  end do

  return
end
subroutine r8rows_to_i4mat ( m, n, r8rows, r8mat )

!*****************************************************************************80
!
!! R8ROWS_TO_R8MAT converts a row-major vector to an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, in column major order.
!
!    I am frustrated that the FORTRAN standard for initializing an array
!    forces me to enter a table of data by columns, so that I have to
!    transpose the information, which is confusing to me and any reader.
!
!    This function allows me to declare a vector of the right type and length,
!    fill it with data that I can display row-wise, and then have the
!    data copied into a FORTRAN array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    integer ( kind = 4 ) R8ROWS(M*N), the data. stored rowwise
!    in a vector.
!
!  Output:
!
!    integer ( kind = 4 ) R8MAT(M,N), a copy of the data, stored
!    columnwise in an array.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8mat(m,n)
  real ( kind = 8 ) r8rows(m*n)

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1
      r8mat(i,j) = r8rows(k)
    end do
  end do

  return
end
subroutine r8slmat_print ( m, n, a, title )

!*****************************************************************************80
!
!! R8SLMAT_PRINT prints a strict lower triangular R8MAT.
!
!  Example:
!
!    M = 5, N = 5
!    A = (/ 21, 31, 41, 51, 32, 42, 52, 43, 53, 54 /)
!
!    21
!    31 32
!    41 42 43
!    51 52 53 54
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of rows in A.
!
!    integer ( kind = 4 ) N, the number of columns in A.
!
!    real ( kind = 8 ) A(*), the M by N matrix.  Only the strict
!    lower triangular elements are stored, in column major order.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(10)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) jmax
  integer ( kind = 4 ) nn
  integer ( kind = 4 ) size
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  jmax = min ( n, m - 1 )

  if ( m-1 <= n ) then
    size = ( m * ( m - 1 ) ) / 2
  else if ( n < m-1 ) then
    size = ( n * ( n - 1 ) ) / 2 + ( m - n - 1 ) * n
  end if

  if ( all ( a(1:size) == aint ( a(1:size) ) ) ) then

    nn = 10

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m - 1, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(a8,10i8)' ) '     Col', ( j, j = jlo, jhi )
      write ( *, '(a8)' )      '     Row'
      do i = jlo + 1, m
        jhi = min ( jlo + nn - 1, i - 1, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j + 1 ) ) / 2
        end do
        write ( *, '(2x,i8,10i8)' ) i, int ( a(indx(1:jhi+1-jlo)) )
      end do
    end do

  else if ( maxval ( abs ( a(1:size) ) ) < 1000000.0D+00 ) then

    nn = 5

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m - 1, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(a10,5(i8,6x))' ) '       Col', ( j, j = jlo, jhi )
      write ( *, '(a10)' )          '       Row'
      do i = jlo + 1, m
        jhi = min ( jlo + nn - 1, i - 1, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j + 1 ) ) / 2
        end do
        write ( *, '(2x,i8,5f14.6)' ) i, a(indx(1:jhi+1-jlo))
      end do
    end do

  else

    nn = 5

    do jlo = 1, jmax, nn
      jhi = min ( jlo + nn - 1, m - 1, jmax )
      write ( *, '(a)' ) ' '
      write ( *, '(a10,5(i8,6x))' ) '       Col', ( j, j = jlo, jhi )
      write ( *, '(a10)' ) '       Row'
      do i = jlo + 1, m
        jhi = min ( jlo + nn - 1, i - 1, jmax )
        do j = jlo, jhi
          indx(j+1-jlo) = ( j - 1 ) * m + i - ( j * ( j + 1 ) ) / 2
        end do
        write ( *, '(2x,i8,5g14.6)' ) i, a(indx(1:jhi+1-jlo))
      end do
    end do

  end if

  return
end
subroutine r8vec_01_to_ab ( n, a, amax, amin )

!*****************************************************************************80
!
!! R8VEC_01_TO_AB shifts and rescales an R8VEC to lie within given bounds.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    On A contains the original data, which is presumed to lie
!    between 0 and 1.  However, it is not necessary that this be so.
!
!    On A has been shifted and rescaled so that all entries which
!    on input lay in [0,1] now lie between AMIN and AMAX.  Other entries will
!    be mapped in a corresponding way.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of data values.
!
!    real ( kind = 8 ) A(N), the vector to be rescaled.
!
!    real ( kind = 8 ) AMAX, AMIN, the maximum and minimum values
!    allowed for A.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the rescaled vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) amax2
  real ( kind = 8 ) amax3
  real ( kind = 8 ) amin
  real ( kind = 8 ) amin2
  real ( kind = 8 ) amin3

  if ( amax == amin ) then
    a(1:n) = amin
    return
  end if

  amax2 = max ( amax, amin )
  amin2 = min ( amax, amin )

  amin3 = minval ( a(1:n) )
  amax3 = maxval ( a(1:n) )

  if ( amax3 /= amin3 ) then

    a(1:n) = ( ( amax3 - a(1:n)         ) * amin2   &
             + (         a(1:n) - amin3 ) * amax2 ) &
             / ( amax3          - amin3 )

  else

    a(1:n) = 0.5D+00 * ( amax2 + amin2 )

  end if

  return
end
function r8vec_amax ( n, a )

!*****************************************************************************80
!
!! R8VEC_AMAX returns the maximum absolute value in an R8VEC.
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
!    16 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_AMAX, the value of the entry
!    of largest magnitude.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_amax
  real ( kind = 8 ) value

  value = maxval ( abs ( a(1:n) ) )

  r8vec_amax = value

  return
end
subroutine r8vec_amax_index ( n, a, amax_index )

!*****************************************************************************80
!
!! R8VEC_AMAX_INDEX returns the index of the maximum absolute value in an R8VEC.
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
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    integer ( kind = 4 ) AMAX_INDEX, the index of the entry of
!    largest magnitude.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax
  integer ( kind = 4 ) amax_index
  integer ( kind = 4 ) i

  if ( n <= 0 ) then

    amax_index = -1

  else

    amax_index = 1
    amax = abs ( a(1) )

    do i = 2, n
      if ( amax < abs ( a(i) ) ) then
        amax_index = i
        amax = abs ( a(i) )
      end if
    end do

  end if

  return
end
function r8vec_amin ( n, a )

!*****************************************************************************80
!
!! R8VEC_AMIN returns the minimum absolute value in an R8VEC.
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
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_AMIN, the value of the entry
!    of smallest magnitude.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_amin
  real ( kind = 8 ) value

  value = minval ( abs ( a(1:n) ) )

  r8vec_amin = value

  return
end
subroutine r8vec_amin_index ( n, a, amin_index )

!*****************************************************************************80
!
!! R8VEC_AMIN_INDEX returns the index of the minimum absolute value in an R8VEC.
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
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    integer ( kind = 4 ) AMIN_INDEX, the index of the entry of
!    smallest magnitude.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amin
  integer ( kind = 4 ) amin_index
  integer ( kind = 4 ) i

  if ( n <= 0 ) then

    amin_index = 0

  else

    amin_index = 1
    amin = abs ( a(1) )

    do i = 2, n
      if ( abs ( a(i) ) < amin ) then
        amin_index = i
        amin = abs ( a(i) )
      end if
    end do

  end if

  return
end
subroutine r8vec_any_normal ( dim_num, v1, v2 )

!*****************************************************************************80
!
!! R8VEC_ANY_NORMAL returns some normal vector to V1.
!
!  Discussion:
!
!    If DIM_NUM < 2, then no normal vector can be returned.
!
!    If V1 is the zero vector, then any unit vector will do.
!
!    No doubt, there are better, more robust algorithms.  But I will take
!    just about ANY reasonable unit vector that is normal to V1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    real ( kind = 8 ) V1(DIM_NUM), the vector.
!
!  Output:
!
!    real ( kind = 8 ) V2(DIM_NUM), a vector that is
!    normal to V2, and has unit Euclidean length.
!
  implicit none

  integer ( kind = 4 ) dim_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) r8vec_norm
  real ( kind = 8 ) v1(dim_num)
  real ( kind = 8 ) v2(dim_num)
  real ( kind = 8 ) vj
  real ( kind = 8 ) vk

  if ( dim_num < 2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_ANY_NORMAL - Fatal error!'
    write ( *, '(a)' ) '  Called with DIM_NUM < 2.'
    stop 1
  end if

  if ( r8vec_norm ( dim_num, v1 ) == 0.0D+00 ) then
    v2(1) = 1.0D+00
    v2(2:dim_num) = 0.0D+00
    return
  end if
!
!  Seek the largest entry in V1, VJ = V1(J), and the
!  second largest, VK = V1(K).
!
!  Since V1 does not have zero norm, we are guaranteed that
!  VJ, at least, is not zero.
!
  j = - 1
  vj = 0.0D+00

  k = - 1
  vk = 0.0D+00

  do i = 1, dim_num

    if ( abs ( vk ) < abs ( v1(i) ) .or. k < 1 ) then

      if ( abs ( vj ) < abs ( v1(i) ) .or. j < 1 ) then
        k = j
        vk = vj
        j = i
        vj = v1(i)
      else
        k = i
        vk = v1(i)
      end if

    end if

  end do
!
!  Setting V2 to zero, except that V2(J) = -VK, and V2(K) = VJ,
!  will just about do the trick.
!
  v2(1:dim_num) = 0.0D+00

  v2(j) = - vk / sqrt ( vk * vk + vj * vj )
  v2(k) =   vj / sqrt ( vk * vk + vj * vj )

  return
end
function r8vec_asum ( n, a )

!*****************************************************************************80
!
!! R8VEC_ASUM returns the sum of the absolute values of the entries of an R8VEC.
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
!    24 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_ASUM, the sum of the absolute
!    values of the entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_asum

  r8vec_asum = sum ( abs ( a(1:n) ) )

  return
end
subroutine r8vec_bin ( n, x, bin_num, bin_min, bin_max, bin, bin_limit )

!*****************************************************************************80
!
!! R8VEC_BIN computes bins based on a given R8VEC.
!
!  Discussion:
!
!    The user specifies minimum and maximum bin values, BIN_MIN and
!    BIN_MAX, and the number of bins, BIN_NUM.  This determines a
!    "bin width":
!
!      H = ( BIN_MAX - BIN_MIN ) / BIN_NUM
!
!    so that bin I will count all entries X(J) such that
!
!      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
!
!    The array X does NOT have to be sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of X.
!
!    real ( kind = 8 ) X(N), an (unsorted) array to be binned.
!
!    integer ( kind = 4 ) BIN_NUM, the number of bins.  Two extra bins,
!    #0 and #BIN_NUM+1, count extreme values.
!
!    real ( kind = 8 ) BIN_MIN, BIN_MAX, define the range and size
!    of the bins.  BIN_MIN and BIN_MAX must be distinct.
!    Normally, BIN_MIN < BIN_MAX, and the documentation will assume
!    this, but proper results will be computed if BIN_MIN > BIN_MAX.
!
!  Output:
!
!    integer ( kind = 4 ) BIN(0:BIN_NUM+1).
!    BIN(0) counts entries of X less than BIN_MIN.
!    BIN(BIN_NUM+1) counts entries greater than or equal to BIN_MAX.
!    For 1 <= I <= BIN_NUM, BIN(I) counts the entries X(J) such that
!      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
!    where H is the bin spacing.
!
!    real ( kind = 8 ) BIN_LIMIT(0:BIN_NUM), the "limits" of the bins.
!    BIN(I) counts the number of entries X(J) such that
!      BIN_LIMIT(I-1) <= X(J) < BIN_LIMIT(I).
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) bin_num

  integer ( kind = 4 ) bin(0:bin_num+1)
  real ( kind = 8 ) bin_limit(0:bin_num)
  real ( kind = 8 ) bin_max
  real ( kind = 8 ) bin_min
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)

  if ( bin_max == bin_min ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_BIN - Fatal error!'
    write ( *, '(a)' ) '  BIN_MIN = BIN_MAX.'
    stop 1
  end if

  bin(0:bin_num+1) = 0

  do i = 1, n

    t = ( x(i) - bin_min ) / ( bin_max - bin_min )

    if ( t < 0.0D+00 ) then
      j = 0
    else if ( 1.0D+00 <= t ) then
      j = bin_num + 1
    else
      j = 1 + int ( real ( bin_num, kind = 8 ) * t )
    end if

    bin(j) = bin(j) + 1

  end do
!
!  Compute the bin limits.
!
  do i = 0, bin_num
    bin_limit(i) = (   real ( bin_num - i, kind = 8 ) * bin_min   &
                     + real (           i, kind = 8 ) * bin_max ) &
                     / real ( bin_num,     kind = 8 )
  end do

  return
end
subroutine r8vec_binary_next ( n, bvec )

!*****************************************************************************80
!
!! R8VEC_BINARY_NEXT generates the next binary vector.
!
!  Discussion:
!
!    The vectors have the order
!
!      (0,0,...,0),
!      (0,0,...,1),
!      ...
!      (1,1,...,1)
!
!    and the "next" vector after (1,1,...,1) is (0,0,...,0).  That is,
!    we allow wrap around.
!
!  Example:
!
!    N = 3
!
!    Input      Output
!    -----      ------
!    0 0 0  =>  0 0 1
!    0 0 1  =>  0 1 0
!    0 1 0  =>  0 1 1
!    0 1 1  =>  1 0 0
!    1 0 0  =>  1 0 1
!    1 0 1  =>  1 1 0
!    1 1 0  =>  1 1 1
!    1 1 1  =>  0 0 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of the vectors.
!
!    real ( kind = 8 ) BVEC(N), the vector whose successor is desired.
!
!  Output:
!
!    real ( kind = 8 ) BVEC(N), the successor to the input vector.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) bvec(n)

  do i = n, 1, -1

    if ( bvec(i) == 0.0D+00 ) then
      bvec(i) = 1.0D+00
      return
    end if

    bvec(i) = 0.0D+00

  end do

  return
end
subroutine r8vec_blend ( n, t1, x1, t2, x2, x )

!*****************************************************************************80
!
!! R8VEC_BLEND performs weighted interpolation of two R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The formula used is:
!
!      x(i) = t * x1(i) + (1-t) * x2(i)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in each  vector.
!
!    real ( kind = 8 ) T1, the weight factor for vector 1.
!
!    real ( kind = 8 ) X1(N), the first vector.
!
!    real ( kind = 8 ) T2, the weight factor for vector 2.
!
!    real ( kind = 8 ) X2(N), the second vector.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the interpolated or extrapolated value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x1(n)
  real ( kind = 8 ) x2(n)

  x(1:n) = t1 * x1(1:n) + t2 * x2(1:n)

  return
end
subroutine r8vec_bracket ( n, x, xval, left, right )

!*****************************************************************************80
!
!! R8VEC_BRACKET searches a sorted R8VEC for successive brackets of a value.
!
!  Discussion:
!
!    This is an inefficient implementation that uses linear search.
!
!    An R8VEC is a vector of R8's.
!
!    If the values in the vector are thought of as defining intervals
!    on the real line, then this routine searches for the interval
!    nearest to or containing the given value.
!
!    It is always true that RIGHT = LEFT+1.
!
!    If XVAL < X(1), then LEFT = 1, RIGHT = 2, and
!      XVAL   < X(1) < X(2);
!    If X(1) <= XVAL < X(N), then
!      X(LEFT) <= XVAL < X(RIGHT);
!    If X(N) <= XVAL, then LEFT = N-1, RIGHT = N, and
!      X(LEFT) <= X(RIGHT) <= XVAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, length of input array.
!
!    real ( kind = 8 ) X(N), an array that has been sorted into
!    ascending order.
!
!    real ( kind = 8 ) XVAL, a value to be bracketed.
!
!  Output:
!
!    integer ( kind = 4 ) LEFT, RIGHT, the results of the search.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) left
  integer ( kind = 4 ) right
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xval

  do i = 2, n - 1

    if ( xval < x(i) ) then
      left = i - 1
      right = i
      return
    end if

   end do

  left = n - 1
  right = n

  return
end
subroutine r8vec_bracket2 ( n, x, xval, start, left, right )

!*****************************************************************************80
!
!! R8VEC_BRACKET2 searches a sorted R8VEC for successive brackets of a value.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If the values in the vector are thought of as defining intervals
!    on the real line, then this routine searches for the interval
!    containing the given value.
!
!    R8VEC_BRACKET2 is a variation on R8VEC_BRACKET.  It seeks to reduce
!    the search time by allowing the user to suggest an interval that
!    probably contains the value.  The routine will look in that interval
!    and the intervals to the immediate left and right.  If this does
!    not locate the point, a binary search will be carried out on
!    appropriate subportion of the sorted array.
!
!    In the most common case, 1 <= LEFT < LEFT + 1 = RIGHT <= N,
!    and X(LEFT) <= XVAL <= X(RIGHT).
!
!    Special cases:
!      Value is less than all data values:
!    LEFT = -1, RIGHT = 1, and XVAL < X(RIGHT).
!      Value is greater than all data values:
!    LEFT = N, RIGHT = -1, and X(LEFT) < XVAL.
!      Value is equal to a data value:
!    LEFT = RIGHT, and X(LEFT) = X(RIGHT) = XVAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, length of the input array.
!
!    real ( kind = 8 ) X(N), an array that has been sorted into
!    ascending order.
!
!    real ( kind = 8 ) XVAL, a value to be bracketed by entries of X.
!
!    integer ( kind = 4 ) START, between 1 and N, specifies that XVAL
!    is likely to be in the interval:
!      [ X(START), X(START+1) ]
!    or, if not in that interval, then either
!      [ X(START+1), X(START+2) ]
!    or
!      [ X(START-1), X(START) ].
!
!  Output:
!
!    integer ( kind = 4 ) LEFT, RIGHT, the results of the search.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) high
  integer ( kind = 4 ) left
  integer ( kind = 4 ) low
  integer ( kind = 4 ) right
  integer ( kind = 4 ) start
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xval
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_BRACKET2 - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    stop 1
  end if

  if ( start < 1 .or. n < start ) then
    start = ( n + 1 ) / 2
  end if
!
!  XVAL = X(START)?
!
  if ( x(start) == xval ) then

    left = start
    right = start
    return
!
!  X(START) < XVAL?
!
  else if ( x(start) < xval ) then
!
!  X(START) = X(N) < XVAL < oo?
!
    if ( n < start + 1 ) then

      left = start
      right = -1
      return
!
!  XVAL = X(START+1)?
!
    else if ( xval == x(start+1) ) then

      left = start + 1
      right = start + 1
      return
!
!  X(START) < XVAL < X(START+1)?
!
    else if ( xval < x(start+1) ) then

      left = start
      right = start + 1
      return
!
!  X(START+1) = X(N) < XVAL < oo?
!
    else if ( n < start + 2 ) then

      left = start + 1
      right = -1
      return
!
!  XVAL = X(START+2)?
!
    else if ( xval == x(start+2) ) then

      left = start + 2
      right = start + 2
      return
!
!  X(START+1) < XVAL < X(START+2)?
!
    else if ( xval < x(start+2) ) then

      left = start + 1
      right = start + 2
      return
!
!  Binary search for XVAL in [ X(START+2), X(N) ],
!  where XVAL is guaranteed to be greater than X(START+2).
!
    else

      low = start + 2
      high = n
      call r8vec_bracket ( high + 1 - low, x(low), xval, left, right )
      left = left + low - 1
      right = right + low - 1

    end if
!
!  -oo < XVAL < X(START) = X(1).
!
  else if ( start == 1 ) then

    left = -1
    right = start
    return
!
!  XVAL = X(START-1)?
!
  else if ( xval == x(start-1) ) then

    left = start - 1
    right = start - 1
    return
!
!  X(START-1) < XVAL < X(START)?
!
  else if ( x(start-1) <= xval ) then

    left = start - 1
    right = start
    return
!
!  Binary search for XVAL in [ X(1), X(START-1) ],
!  where XVAL is guaranteed to be less than X(START-1).
!
  else

    low = 1
    high = start - 1
    call r8vec_bracket ( high + 1 - low, x(1), xval, left, right )

  end if

  return
end
subroutine r8vec_bracket3 ( n, t, tval, left )

!*****************************************************************************80
!
!! R8VEC_BRACKET3 finds the interval containing or nearest a given value.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine always returns the index LEFT of the sorted array
!    T with the property that either
!    *  T is contained in the interval [ T(LEFT), T(LEFT+1) ], or
!    *  T < T(LEFT) = T(1), or
!    *  T > T(LEFT+1) = T(N).
!
!    The routine is useful for interpolation problems, where
!    the abscissa must be located within an interval of data
!    abscissas for interpolation, or the "nearest" interval
!    to the (extreme) abscissa must be found so that extrapolation
!    can be carried out.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, length of the input array.
!
!    real ( kind = 8 ) T(N), an array that has been sorted
!    into ascending order.
!
!    real ( kind = 8 ) TVAL, a value to be bracketed by entries of T.
!
!    integer ( kind = 4 ) LEFT.
!    if 1 <= LEFT <= N-1, LEFT is taken as a suggestion for the
!    interval [ T(LEFT), T(LEFT+1) ] in which TVAL lies.  This interval
!    is searched first, followed by the appropriate interval to the left
!    or right.  After that, a binary search is used.
!
!  Output:
!
!    integer ( kind = 4 ) LEFT, set so that the interval [ T(LEFT), T(LEFT+1) ]
!    is the closest to TVAL; it either contains TVAL, or else TVAL
!    lies outside the interval [ T(1), T(N) ].
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) high
  integer ( kind = 4 ) left
  integer ( kind = 4 ) low
  integer ( kind = 4 ) mid
  real ( kind = 8 ) t(n)
  real ( kind = 8 ) tval
!
!  Check the input data.
!
  if ( n < 2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_BRACKET3 - Fatal error!'
    write ( *, '(a)' ) '  N must be at least 2.'
    stop 1
  end if
!
!  If LEFT is not between 1 and N-1, set it to the middle value.
!
  if ( left < 1 .or. n - 1 < left ) then
    left = ( n + 1 ) / 2
  end if
!
!  CASE 1: TVAL < T(LEFT):
!  Search for TVAL in [T(I), T(I+1)] for intervals I = 1 to LEFT-1.
!
  if ( tval < t(left) ) then

    if ( left == 1 ) then
      return
    else if ( left == 2 ) then
      left = 1
      return
    else if ( t(left-1) <= tval ) then
      left = left - 1
      return
    else if ( tval <= t(2) ) then
      left = 1
      return
    end if
!
!  ...Binary search for TVAL in [T(I), T(I+1)] for intervals I = 2 to LEFT-2.
!
    low = 2
    high = left - 2

    do

      if ( low == high ) then
        left = low
        return
      end if

      mid = ( low + high + 1 ) / 2

      if ( t(mid) <= tval ) then
        low = mid
      else
        high = mid - 1
      end if

    end do
!
!  CASE2: T(LEFT+1) < TVAL:
!  Search for TVAL in [T(I),T(I+1)] for intervals I = LEFT+1 to N-1.
!
  else if ( t(left+1) < tval ) then

    if ( left == n - 1 ) then
      return
    else if ( left == n - 2 ) then
      left = left + 1
      return
    else if ( tval <= t(left+2) ) then
      left = left + 1
      return
    else if ( t(n-1) <= tval ) then
      left = n - 1
      return
    end if
!
!  ...Binary search for TVAL in [T(I), T(I+1)] for intervals I = LEFT+2 to N-2.
!
    low = left + 2
    high = n - 2

    do

      if ( low == high ) then
        left = low
        return
      end if

      mid = ( low + high + 1 ) / 2

      if ( t(mid) <= tval ) then
        low = mid
      else
        high = mid - 1
      end if

    end do
!
!  CASE3: T(LEFT) <= TVAL <= T(LEFT+1):
!  T is in [T(LEFT), T(LEFT+1)], as the user said it might be.
!
  else

  end if

  return
end
subroutine r8vec_bracket4 ( nt, t, ns, s, left )

!*****************************************************************************80
!
!! R8VEC_BRACKET4 finds the nearest interval to each of a vector of values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine always returns the index LEFT of the sorted array
!    T with the property that either
!    *  T is contained in the interval [ T(LEFT), T(LEFT+1) ], or
!    *  T < T(LEFT) = T(1), or
!    *  T > T(LEFT+1) = T(NT).
!
!    The routine is useful for interpolation problems, where
!    the abscissa must be located within an interval of data
!    abscissas for interpolation, or the "nearest" interval
!    to the (extreme) abscissa must be found so that extrapolation
!    can be carried out.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) NT, length of the input array.
!
!    real ( kind = 8 ) T(NT), an array that has been sorted
!    into ascending order.
!
!    integer ( kind = 4 ) NS, the number of points to be bracketed.
!
!    real ( kind = 8 ) S(NS), values to be bracketed by entries of T.
!
!  Output:
!
!    integer ( kind = 4 ) LEFT(NS).
!    LEFT(I) is set so that the interval [ T(LEFT(I)), T(LEFT(I)+1) ]
!    is the closest to S(I); it either contains S(I), or else S(I)
!    lies outside the interval [ T(1), T(NT) ].
!
  implicit none

  integer ( kind = 4 ) ns
  integer ( kind = 4 ) nt

  integer ( kind = 4 ) high
  integer ( kind = 4 ) i
  integer ( kind = 4 ) left(ns)
  integer ( kind = 4 ) low
  integer ( kind = 4 ) mid
  real ( kind = 8 ) s(ns)
  real ( kind = 8 ) t(nt)
!
!  Check the input data.
!
  if ( nt < 2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_BRACKET4 - Fatal error!'
    write ( *, '(a)' ) '  NT must be at least 2.'
    stop 1
  end if

  do i = 1, ns

    left(i) = ( nt + 1 ) / 2
!
!  CASE 1: S < T(LEFT):
!  Search for S in [T(I), T(I+1)] for intervals I = 1 to LEFT-1.
!
    if ( s(i) < t(left(i)) ) then

      if ( left(i) == 1 ) then
        cycle
      else if ( left(i) == 2 ) then
        left(i) = 1
        cycle
      else if ( t(left(i)-1) <= s(i) ) then
        left(i) = left(i) - 1
        cycle
      else if ( s(i) <= t(2) ) then
        left(i) = 1
        cycle
      end if
!
!  ...Binary search for S in [T(I), T(I+1)] for intervals I = 2 to LEFT-2.
!
      low = 2
      high = left(i) - 2

      do

        if ( low == high ) then
          left(i) = low
          exit
        end if

        mid = ( low + high + 1 ) / 2

        if ( t(mid) <= s(i) ) then
          low = mid
        else
          high = mid - 1
        end if

      end do
!
!  CASE2: T(LEFT+1) < S:
!  Search for S in [T(I),T(I+1)] for intervals I = LEFT+1 to N-1.
!
    else if ( t(left(i)+1) < s(i) ) then

      if ( left(i) == nt - 1 ) then
        cycle
      else if ( left(i) == nt - 2 ) then
        left(i) = left(i) + 1
        cycle
      else if ( s(i) <= t(left(i)+2) ) then
        left(i) = left(i) + 1
        cycle
      else if ( t(nt-1) <= s(i) ) then
        left(i) = nt - 1
        cycle
      end if
!
!  ...Binary search for S in [T(I), T(I+1)] for intervals I = LEFT+2 to NT-2.
!
      low = left(i) + 2
      high = nt - 2

      do

        if ( low == high ) then
          left(i) = low
          exit
        end if

        mid = ( low + high + 1 ) / 2

        if ( t(mid) <= s(i) ) then
          low = mid
        else
          high = mid - 1
        end if

      end do
!
!  CASE3: T(LEFT) <= S <= T(LEFT+1):
!  S is in [T(LEFT), T(LEFT+1)].
!
    else

    end if

  end do

  return
end
function r8vec_bracket5 ( nd, xd, xi )

!*****************************************************************************80
!
!! R8VEC_BRACKET5 brackets data between successive entries of a sorted R8VEC.
!
!  Discussion:
!
!    We assume XD is sorted.
!
!    If XI is contained in the interval [XD(1),XD(N)], then the returned 
!    value B indicates that XI is contained in [ XD(B), XD(B+1) ].
!
!    If XI is not contained in the interval [XD(1),XD(N)], then B = -1.
!
!    This code implements a version of binary search which is perhaps more
!    understandable than the usual ones.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) ND, the number of data values.
!
!    real ( kind = 8 ) XD(N), the sorted data.
!
!    real ( kind = 8 ) XD, the query value.
!
!  Output:
!
!    integer ( kind = 4 ) R8VEC_BRACKET5, the bracket information.
!
  implicit none

  integer ( kind = 4 ) nd

  integer ( kind = 4 ) b
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) r
  integer ( kind = 4 ) r8vec_bracket5
  real ( kind = 8 ) xd(nd)
  real ( kind = 8 ) xi

  if ( xi < xd(1) .or. xd(nd) < xi ) then

    b = -1

  else

    l = 1
    r = nd

    do while ( l + 1 < r )
      m = ( l + r ) / 2
      if ( xi < xd(m) ) then
        r = m
      else
        l = m
      end if
    end do

    b = l

  end if

  r8vec_bracket5 = b

  return
end
subroutine r8vec_bracket6 ( nd, xd, ni, xi, b )

!*****************************************************************************80
!
!! R8VEC_BRACKET6 brackets data between successive entries of a sorted R8VEC.
!
!  Discussion:
!
!    We assume XD is sorted.
!
!    If XI(I) is contained in the interval [XD(1),XD(N)], then the value of
!    B(I) indicates that XI(I) is contained in [ XD(B(I)), XD(B(I)+1) ].
!
!    If XI(I) is not contained in the interval [XD(1),XD(N)], then B(I) = -1.
!
!    This code implements a version of binary search which is perhaps more
!    understandable than the usual ones.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) ND, the number of data values.
!
!    real ( kind = 8 ) XD(N), the sorted data.
!
!    integer ( kind = 4 ) NI, the number of inquiry values.
!
!    real ( kind = 8 ) XD(NI), the query values.
!
!  Output:
!
!    integer ( kind = 4 ) B(NI), the bracket information.
!
  implicit none

  integer ( kind = 4 ) nd
  integer ( kind = 4 ) ni

  integer ( kind = 4 ) b(ni)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) r
  real ( kind = 8 ) xd(nd)
  real ( kind = 8 ) xi(ni)

  do i = 1, ni

    if ( xi(i) < xd(1) .or. xd(nd) < xi(i) ) then

      b(i) = -1

    else

      l = 1
      r = nd

      do while ( l + 1 < r )
        m = ( l + r ) / 2
        if ( xi(i) < xd(m) ) then
          r = m
        else
          l = m
        end if
      end do

      b(i) = l

    end if

  end do

  return
end
subroutine r8vec_ceiling ( n, r8vec, ceilingvec )

!*****************************************************************************80
!
!! R8VEC_CEILING rounds "up" (towards +oo) entries of an R8VEC.
!
!  Example:
!
!    R8    Value
!
!   -1.1  -1.0
!   -1.0  -1.0
!   -0.9   0.0
!    0.0   0.0
!    5.0   5.0
!    5.1   6.0
!    5.9   6.0
!    6.0   6.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries.
!
!    real ( kind = 8 ) R8VEC(N), the vector.
!
!  Output:
!
!    real ( kind = 8 ) CEILINGVEC(N), the rounded values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) ceilingvec(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec(n)
  real ( kind = 8 ) value

  do i = 1, n

    value = real ( int ( r8vec(i) ), kind = 8 )

    if ( value < r8vec(i) ) then
      value = value + 1.0D+00
    end if

    ceilingvec(i) = value

  end do

  return
end
subroutine r8vec_cheby_extreme ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_CHEBY_EXTREME creates Chebyshev Extreme values in [A,B].
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
!    17 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A, B, the first and last entries.
!
!  Output:
!
!    real ( kind = 8 ) X(N), a vector of Chebyshev spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) theta
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n

      theta = real ( n - i, kind = 8 ) * r8_pi / real ( n - 1, kind = 8 )

      c = cos ( theta )

      if ( mod ( n, 2 ) == 1 ) then
        if ( 2 * i - 1 == n ) then
          c = 0.0D+00
        end if
      end if

      x(i) = ( ( 1.0D+00 - c ) * a   &
             + ( 1.0D+00 + c ) * b ) &
             /   2.0D+00

    end do

  end if

  return
end
subroutine r8vec_cheby_zero ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_CHEBY_ZERO creates Chebyshev Zero values in [A,B].
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
!    17 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A, B, the first and last entries.
!
!  Output:
!
!    real ( kind = 8 ) X(N), a vector of Chebyshev spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) theta
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n

      theta = real ( 2 * ( n - i ) + 1, kind = 8 ) * r8_pi &
        / real ( 2 * n, kind = 8 )

      c = cos ( theta )

      if ( mod ( n, 2 ) == 1 ) then
        if ( 2 * i - 1 == n ) then
          c = 0.0D+00
        end if
      end if

      x(i) = ( ( 1.0D+00 - c ) * a   &
             + ( 1.0D+00 + c ) * b ) &
             /   2.0D+00

    end do

  end if

  return
end
subroutine r8vec_cheby1space ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_CHEBY1SPACE creates Type 1 Chebyshev-spaced values in [A,B].
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
!    17 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A, B, the first and last entries.
!
!  Output:
!
!    real ( kind = 8 ) X(N), a vector of Type 1 Chebyshev spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) theta
  real ( kind = 8 ) x(n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    do i = 1, n

      theta = real ( n - i, kind = 8 ) * r8_pi / real ( n - 1, kind = 8 )

      c = cos ( theta )

      if ( mod ( n, 2 ) == 1 ) then
        if ( 2 * i - 1 == n ) then
          c = 0.0D+00
        end if
      end if

      x(i) = ( ( 1.0D+00 - c ) * a   &
             + ( 1.0D+00 + c ) * b ) &
             /   2.0D+00

    end do

  end if

  return
end
subroutine r8vec_cheby2space ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_CHEBY2SPACE creates Type 2 Chebyshev-spaced values in [A,B].
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
!    05 July 2017
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A, B, the first and last entries.
!
!  Output:
!
!    real ( kind = 8 ) X(N), a vector of Type 2 Chebyshev spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) theta
  real ( kind = 8 ) x(n)

  do i = 1, n

    theta = real ( n - i, kind = 8 ) * r8_pi / real ( n + 1, kind = 8 )

    c = cos ( theta )

    x(i) = ( ( 1.0D+00 - c ) * a   &
           + ( 1.0D+00 + c ) * b ) &
           /   2.0D+00

  end do

  return
end
subroutine r8vec_compare ( n, a1, a2, isgn )

!*****************************************************************************80
!
!! R8VEC_COMPARE compares two R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The lexicographic ordering is used.
!
!  Example:
!
!    Input:
!
!      A1 = ( 2.0, 6.0, 2.0 )
!      A2 = ( 2.0, 8.0, 12.0 )
!
!    Output:
!
!      ISGN = -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    real ( kind = 8 ) A1(N), A2(N), the vectors to be compared.
!
!  Output:
!
!    integer ( kind = 4 ) ISGN, the results of the comparison:
!    -1, A1 < A2,
!     0, A1 = A2,
!    +1, A1 > A2.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) k

  isgn = 0

  k = 1

  do while ( k <= n )

    if ( a1(k) < a2(k) ) then
      isgn = -1
      return
    else if ( a2(k) < a1(k) ) then
      isgn = + 1
      return
    end if

    k = k + 1

  end do

  return
end
subroutine r8vec_concatenate ( n1, a, n2, b, c )

!*****************************************************************************80
!
!! R8VEC_CONCATENATE concatenates two R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N1, the number of entries in the first vector.
!
!    real ( kind = 8 ) A(N1), the first vector.
!
!    integer ( kind = 4 ) N2, the number of entries in the second vector.
!
!    real ( kind = 8 ) B(N2), the second vector.
!
!  Output:
!
!    real ( kind = 8 ) C(N1+N2), the concatenation of A and B.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  real ( kind = 8 ) a(n1)
  real ( kind = 8 ) b(n2)
  real ( kind = 8 ) c(n1+n2)
  
  c(   1:n1)    = a(1:n1)
  c(n1+1:n1+n2) = b(1:n2)

  return
end
subroutine r8vec_convolution ( m, x, n, y, z )

!*****************************************************************************80
!
!! R8VEC_CONVOLUTION returns the convolution of two R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The I-th entry of the convolution can be formed by summing the products 
!    that lie along the I-th diagonal of the following table:
!
!    Y3 | 3   4   5   6   7
!    Y2 | 2   3   4   5   6
!    Y1 | 1   2   3   4   5
!       +------------------
!        X1  X2  X3  X4  X5
!
!    which will result in:
!
!    Z = ( X1 * Y1,
!          X1 * Y2 + X2 * Y1,
!          X1 * Y3 + X2 * Y2 + X3 * Y1,
!                    X2 * Y3 + X3 * Y2 + X4 * Y1,
!                              X3 * Y3 + X4 * Y2 + X5 * Y1,
!                                        X4 * Y3 + X5 * Y2,
!                                                  X5 * Y3 )
!            
!  Example:
!
!    Input:
!
!      X = (/ 1, 2, 3, 4 /)
!      Y = (/ -1, 5, 3 /)
!
!    Output:
!
!      Z = (/ -1, 3, 10, 17, 29, 12 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 May 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the dimension of X.
!
!    real ( kind = 8 ) X(M), the first vector to be convolved.
!
!    integer ( kind = 4 ) N, the dimension of Y.
!
!    real ( kind = 8 ) Y(N), the second vector to be convolved.
!
!  Output:
!
!    real ( kind = 8 ) Z(M+N-1), the convolution of X and Y.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  real ( kind = 8 ) x(m)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(m+n-1)

  z(1:m+n-1) = 0.0D+00

  do j = 1, n
    z(j:j+m-1) = z(j:j+m-1) + x(1:m) * y(j)
  end do

  return
end
subroutine r8vec_convolution_circ ( n, x, y, z )

!*****************************************************************************80
!
!! R8VEC_CONVOLUTION_CIRC: discrete circular convolution of two R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The formula used is:
!
!      z(1+m) = xCCy(m) = sum ( 0 <= k <= n-1 ) x(1+k) * y(1+m-k)
!
!    Here, if the index of Y becomes nonpositive, it is "wrapped around"
!    by having N added to it.
!
!    The circular convolution is equivalent to multiplication of Y by a
!    circulant matrix formed from the vector X.
!
!  Example:
!
!    Input:
!
!      X = (/ 1, 2, 3, 4 /)
!      Y = (/ 1, 2, 4, 8 /)
!
!    Output:
!
!      Circulant form:
!
!      Z = ( 1 4 3 2 )   ( 1 )
!          ( 2 1 4 3 )   ( 2 )
!          ( 3 2 1 4 ) * ( 4 )
!          ( 4 3 2 1 )   ( 8 )
!
!      The formula:
!
!      Z = (/ 1*1 + 2*8 + 3*4 + 4*2,
!             1*2 + 2*1 + 3*8 + 4*4,
!             1*4 + 2*2 + 3*1 + 4*8,
!             1*8 + 2*4 + 3*2 + 4*1 /)
!
!      Result:
!
!      Z = (/ 37, 44, 43, 26 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 September 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of the vectors.
!
!    real ( kind = 8 ) X(N), Y(N), the vectors to be convolved.
!
!  Output:
!
!    real ( kind = 8 ) Z(N), the circular convolution of X and Y.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) m
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(n)

  do m = 1, n
    z(m) = dot_product ( x(1:m), y(m:1:-1) ) &
         + dot_product ( x(m+1:n), y(n:m+1:-1) )
  end do

  return
end
subroutine r8vec_copy ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_COPY copies an R8VEC.
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
!    17 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the length of the vectors.
!
!    real ( kind = 8 ) A1(N), the vector to be copied.
!
!  Output:
!
!    real ( kind = 8 ) A2(N), a copy of A1.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)

  a2(1:n) = a1(1:n)

  return
end
subroutine r8vec_correlation ( n, x, y, r )

!*****************************************************************************80
!
!! r8vec_correlation returns the correlation of two R8VEC's.
!
!  Discussion:
!
!    The correlation coefficient is also known as Pearson's r coefficient.
!
!    It must be the case that -1 <= r <= +1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 August 2019
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of the vectors.
!
!    real ( kind = 8 ) X(N), Y(N), the vectors.
!
!  Output:
!
!    real ( kind = 8 ) r, the correlation of X and Y.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) r
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_centered(n)
  real ( kind = 8 ) x_mean
  real ( kind = 8 ) x_std
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) y_centered(n)
  real ( kind = 8 ) y_mean
  real ( kind = 8 ) y_std

  if ( n <= 1 ) then

    r = 0.0D+00

  else

    call r8vec_mean ( n, x, x_mean )
    call r8vec_std_sample ( n, x, x_std )
    x_centered = x - x_mean

    call r8vec_mean ( n, y, y_mean )
    call r8vec_std_sample ( n, y, y_std )
    y_centered = y - y_mean
 
    r = dot_product ( x_centered, y_centered ) / x_std / y_std / real ( n - 1, kind = 8 )

  end if

  return
end
function r8vec_covariance ( n, x, y )

!*****************************************************************************80
!
!! R8VEC_COVARIANCE computes the covariance of two vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2013
!
!  Author:
!
!    John Burkardt.
!
!  Input:
!
!    real ( kind = 8 ) X(N), Y(N), the two vectors.
!
!    integer ( kind = 4 ) N, the dimension of the two vectors.
!
!  Output:
!
!    real ( kind = 8 ) R4VEC_COVARIANCE, the covariance of the vectors.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec_covariance
  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_average
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) y_average

  x_average = sum ( x(1:n) ) / real ( n, kind = 8 )
  y_average = sum ( y(1:n) ) / real ( n, kind = 8 )
 
  value = 0.0D+00
  do i = 1, n
    value = value + ( x(i) - x_average ) * ( y(i) - y_average )
  end do

  r8vec_covariance = value / real ( n - 1, kind = 8 )

  return
end
function r8vec_cross_product_2d ( v1, v2 )

!*****************************************************************************80
!
!! R8VEC_CROSS_PRODUCT_2D finds the cross product of a pair of vectors in 2D.
!
!  Discussion:
!
!    Strictly speaking, the vectors V1 and V2 should be considered
!    to lie in a 3D space, both having Z coordinate zero.  The cross
!    product value V3 then represents the standard cross product vector
!    (0,0,V3).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) V1(2), V2(2), the vectors.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_CROSS_PRODUCT_2D, the cross product.
!
  implicit none

  real ( kind = 8 ) r8vec_cross_product_2d
  real ( kind = 8 ) v1(2)
  real ( kind = 8 ) v2(2)

  r8vec_cross_product_2d = v1(1) * v2(2) - v1(2) * v2(1)

  return
end
function r8vec_cross_product_affine_2d ( v0, v1, v2 )

!*****************************************************************************80
!
!! R8VEC_CROSS_PRODUCT_AFFINE_2D finds the affine cross product in 2D.
!
!  Discussion:
!
!    Strictly speaking, the vectors V1 and V2 should be considered
!    to lie in a 3D space, both having Z coordinate zero.  The cross
!    product value V3 then represents the standard cross product vector
!    (0,0,V3).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) V0(2), the base vector.
!
!    real ( kind = 8 ) V1(2), V2(2), the vectors.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_CROSS_PRODUCT_AFFINE_2D,
!    the cross product (V1-V0) x (V2-V0).
!
  implicit none

  real ( kind = 8 ) r8vec_cross_product_affine_2d
  real ( kind = 8 ) v0(2)
  real ( kind = 8 ) v1(2)
  real ( kind = 8 ) v2(2)

  r8vec_cross_product_affine_2d = &
      ( v1(1) - v0(1) ) * ( v2(2) - v0(2) ) &
    - ( v2(1) - v0(1) ) * ( v1(2) - v0(2) )

  return
end
subroutine r8vec_cross_product_3d ( v1, v2, v3 )

!*****************************************************************************80
!
!! R8VEC_CROSS_PRODUCT_3D computes the cross product of two R8VEC's in 3D.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The cross product in 3D can be regarded as the determinant of the
!    symbolic matrix:
!
!          |  i  j  k |
!      det | x1 y1 z1 |
!          | x2 y2 z2 |
!
!      = ( y1 * z2 - z1 * y2 ) * i
!      + ( z1 * x2 - x1 * z2 ) * j
!      + ( x1 * y2 - y1 * x2 ) * k
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) V1(3), V2(3), the two vectors.
!
!  Output:
!
!    real ( kind = 8 ) V3(3), the cross product vector.
!
  implicit none

  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)

  v3(1) = v1(2) * v2(3) - v1(3) * v2(2)
  v3(2) = v1(3) * v2(1) - v1(1) * v2(3)
  v3(3) = v1(1) * v2(2) - v1(2) * v2(1)

  return
end
subroutine r8vec_cross_product_affine_3d ( v0, v1, v2, v3 )

!*****************************************************************************80
!
!! R8VEC_CROSS_PRODUCT_AFFINE_3D computes the affine cross product in 3D.
!
!  Discussion:
!
!    The cross product in 3D can be regarded as the determinant of the
!    symbolic matrix:
!
!          |  i  j  k |
!      det | x1 y1 z1 |
!          | x2 y2 z2 |
!
!      = ( y1 * z2 - z1 * y2 ) * i
!      + ( z1 * x2 - x1 * z2 ) * j
!      + ( x1 * y2 - y1 * x2 ) * k
!
!    Here, we use V0 as the base of an affine system so we compute
!    the cross product of (V1-V0) and (V2-V0).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) V0(3), the base vector.
!
!    real ( kind = 8 ) V1(3), V2(3), the two vectors.
!
!  Output:
!
!    real ( kind = 8 ) V3(3), the cross product vector
!    ( V1-V0) x (V2-V0).
!
  implicit none

  real ( kind = 8 ) v0(3)
  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)

  v3(1) = ( v1(2) - v0(2) ) * ( v2(3) - v0(3) ) &
        - ( v2(2) - v0(2) ) * ( v1(3) - v0(3) )

  v3(2) = ( v1(3) - v0(3) ) * ( v2(1) - v0(1) ) &
        - ( v2(3) - v0(3) ) * ( v1(1) - v0(1) )

  v3(3) = ( v1(1) - v0(1) ) * ( v2(2) - v0(2) ) &
        - ( v2(1) - v0(1) ) * ( v1(2) - v0(2) )

  return
end
subroutine r8vec_cum ( n, a, a_cum )

!*****************************************************************************80
!
!! R8VEC_CUM computes the cumulutive sums of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Input:
!
!      A = (/ 1.0, 2.0, 3.0, 4.0 /)
!
!    Output:
!
!      A_CUM = (/ 1.0, 3.0, 6.0, 10.0 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A(N), the vector to be summed.
!
!  Output:
!
!    real ( kind = 8 ) A_CUM(1:N), the cumulative sums.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_cum(n)
  integer ( kind = 4 ) i

  a_cum(1) = a(1)

  do i = 2, n
    a_cum(i) = a_cum(i-1) + a(i)
  end do

  return
end
subroutine r8vec_cum0 ( n, a, a_cum )

!*****************************************************************************80
!
!! R8VEC_CUM0 computes the cumulutive sums of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Input:
!
!      A = (/ 1.0, 2.0, 3.0, 4.0 /)
!
!    Output:
!
!      A_CUM = (/ 0.0, 1.0, 3.0, 6.0, 10.0 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A(N), the vector to be summed.
!
!  Output:
!
!    real ( kind = 8 ) A_CUM(0:N), the cumulative sums.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_cum(0:n)
  integer ( kind = 4 ) i

  a_cum(0) = 0.0D+00

  do i = 1, n
    a_cum(i) = a_cum(i-1) + a(i)
  end do

  return
end
subroutine r8vec_dif ( n, h, cof )

!*****************************************************************************80
!
!! R8VEC_DIF computes coefficients for estimating the N-th derivative.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine computes the N+1 coefficients for a centered finite difference
!    estimate of the N-th derivative of a function.
!
!    The estimate has the form
!
!      FDIF(N,X) = Sum (I = 0 to N) COF(I) * F ( X(I) )
!
!    To understand the computation of the coefficients, it is enough
!    to realize that the first difference approximation is
!
!      FDIF(1,X) = F(X+DX) - F(X-DX) ) / (2*DX)
!
!    and that the second difference approximation can be regarded as
!    the first difference approximation repeated:
!
!      FDIF(2,X) = FDIF(1,X+DX) - FDIF(1,X-DX) / (2*DX)
!         = F(X+2*DX) - 2 F(X) + F(X-2*DX) / (4*DX)
!
!    and so on for higher order differences.
!
!    Thus, the next thing to consider is the integer coefficients of
!    the sampled values of F, which are clearly the Pascal coefficients,
!    but with an alternating negative sign.  In particular, if we
!    consider row I of Pascal's triangle to have entries j = 0 through I,
!    then P(I,J) = P(I-1,J-1) - P(I-1,J), where P(*,-1) is taken to be 0,
!    and P(0,0) = 1.
!
!       1
!      -1  1
!       1 -2   1
!      -1  3  -3   1
!       1 -4   6  -4   1
!      -1  5 -10  10  -5  1
!       1 -6  15 -20  15 -6 1
!
!    Next, note that the denominator of the approximation for the
!    N-th derivative will be (2*DX)^N.
!
!    And finally, consider the location of the N+1 sampling
!    points for F:
!
!      X-N*DX, X-(N-2)*DX, X-(N-4)*DX, ..., X+(N-4)*DX, X+(N-2*DX), X+N*DX.
!
!    Thus, a formula for evaluating FDIF(N,X) is
!
!      fdif = 0.0
!      do i = 0, n
!        xi = x + (2*i-n) * h
!        fdif = fdif + cof(i) * f(xi)
!      end do
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 February 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the derivative to be
!    approximated.  N must be 0 or greater.
!
!    real ( kind = 8 ) H, the half spacing between points.
!    H must be positive.
!
!  Output:
!
!    real ( kind = 8 ) COF(0:N), the coefficients needed to approximate
!    the N-th derivative of a function F.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) cof(0:n)
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  if ( n < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_DIF - Fatal error!'
    write ( *, '(a,i8)' ) '  Derivative order N = ', n
    write ( *, '(a)' ) '  but N must be at least 0.'
    stop 1
  end if

  if ( h <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_DIF - Fatal error!'
    write ( *, '(a,g14.6)' ) '  The half sampling spacing is H = ', h
    write ( *, '(a)' ) '  but H must be positive.'
    stop 1
  end if

  do i = 0, n

    cof(i) = 1.0D+00

    do j = i - 1, 1, -1
      cof(j) = - cof(j) + cof(j-1)
    end do

    if ( 0 < i ) then
      cof(0) = - cof(0)
    end if

  end do

  cof(0:n) = cof(0:n) / ( 2.0D+00 * h ) ** n

  return
end
function r8vec_diff_dot_product ( n, u1, v1, u2, v2 )

!*****************************************************************************80
!
!! R8VEC_DIFF_DOT_PRODUCT: dot product of a pair of R8VEC differences.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of the vectors.
!
!    real ( kind = 8 ) U1(N), V1(N), defines the vector U1-V1.
!
!    real ( kind = 8 ) U2(N), V2(N), defines the vector U2-V2.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_DIFF_DOT_PRODUCT, the dot product 
!    of (U1-V1)*(U2-V2).
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec_diff_dot_product
  real ( kind = 8 ) u1(n)
  real ( kind = 8 ) u2(n)
  real ( kind = 8 ) v1(n)
  real ( kind = 8 ) v2(n)
  real ( kind = 8 ) value

  value = 0.0D+00
  do i = 1, n
    value = value + ( u1(i) - v1(i) ) * ( u2(i) - v2(i) )
  end do

  r8vec_diff_dot_product = value

  return
end
function r8vec_diff_norm ( n, a, b )

!*****************************************************************************80
!
!! R8VEC_DIFF_NORM returns the L2 norm of the difference of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L2 norm is defined as:
!
!      R8VEC_NORM_L2 = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), B(N), the vectors
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_DIFF_NORM, the L2 norm of A - B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) r8vec_diff_norm

  r8vec_diff_norm = sqrt ( sum ( ( a(1:n) - b(1:n) ) ** 2 ) )

  return
end
function r8vec_diff_norm_l1 ( n, a, b )

!*****************************************************************************80
!
!! R8VEC_DIFF_NORM_L1 returns the L1 norm of the difference of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L1 norm is defined as:
!
!      R8VEC_NORM_L1 = sum ( 1 <= I <= N ) abs ( A(I) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), B(N), the vectors.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_DIFF_NORM_L1, the L1 norm of A - B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) r8vec_diff_norm_l1

  r8vec_diff_norm_l1 = sum ( abs ( a(1:n) - b(1:n) ) )

  return
end
function r8vec_diff_norm_l2 ( n, a, b )

!*****************************************************************************80
!
!! R8VEC_DIFF_NORM_L2 returns the L2 norm of the difference of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L2 norm is defined as:
!
!      R8VEC_NORM_L2 = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), B(N), the vectors
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_DIFF_NORM_L2, the L2 norm of A - B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) r8vec_diff_norm_l2

  r8vec_diff_norm_l2 = sqrt ( sum ( ( a(1:n) - b(1:n) ) ** 2 ) )

  return
end
function r8vec_diff_norm_li ( n, a, b )

!*****************************************************************************80
!
!! R8VEC_DIFF_NORM_LI returns the Loo norm of the difference of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector Loo norm is defined as:
!
!      R8VEC_NORM_LI = max ( 1 <= I <= N ) abs ( A(I) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), B(N), the vectors
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_DIFF_NORM_LI, the Loo norm of A - B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) r8vec_diff_norm_li

  r8vec_diff_norm_li = maxval ( abs ( a(1:n) - b(1:n) ) )

  return
end
function r8vec_diff_norm_squared ( n, a, b )

!*****************************************************************************80
!
!! R8VEC_DIFF_NORM_SQUARED: square of the L2 norm of the difference of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    R8VEC_DIFF_NORM_SQUARED = sum ( 1 <= I <= N ) ( A(I) - B(I) )^2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), B(N), the vectors
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_DIFF_NORM_SQUARED, the square of 
!    the L2 norm of A - B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) r8vec_diff_norm_squared

  r8vec_diff_norm_squared = sum ( ( a(1:n) - b(1:n) ) ** 2 )

  return
end
subroutine r8vec_direct_product ( factor_index, factor_order, factor_value, &
  factor_num, point_num, x )

!*****************************************************************************80
!
!! R8VEC_DIRECT_PRODUCT creates a direct product of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    To explain what is going on here, suppose we had to construct
!    a multidimensional quadrature rule as the product of K rules
!    for 1D quadrature.
!
!    The product rule will be represented as a list of points and weights.
!
!    The J-th item in the product rule will be associated with
!      item J1 of 1D rule 1,
!      item J2 of 1D rule 2,
!      ...,
!      item JK of 1D rule K.
!
!    In particular,
!      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
!    and
!      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)
!
!    So we can construct the quadrature rule if we can properly
!    distribute the information in the 1D quadrature rules.
!
!    This routine carries out that task for the abscissas X.
!
!    Another way to do this would be to compute, one by one, the
!    set of all possible indices (J1,J2,...,JK), and then index
!    the appropriate information.  An advantage of the method shown
!    here is that you can process the K-th set of information and
!    then discard it.
!
!  Example:
!
!    Rule 1:
!      Order = 4
!      X(1:4) = ( 1, 2, 3, 4 )
!
!    Rule 2:
!      Order = 3
!      X(1:3) = ( 10, 20, 30 )
!
!    Rule 3:
!      Order = 2
!      X(1:2) = ( 100, 200 )
!
!    Product Rule:
!      Order = 24
!      X(1:24) =
!        ( 1, 10, 100 )
!        ( 2, 10, 100 )
!        ( 3, 10, 100 )
!        ( 4, 10, 100 )
!        ( 1, 20, 100 )
!        ( 2, 20, 100 )
!        ( 3, 20, 100 )
!        ( 4, 20, 100 )
!        ( 1, 30, 100 )
!        ( 2, 30, 100 )
!        ( 3, 30, 100 )
!        ( 4, 30, 100 )
!        ( 1, 10, 200 )
!        ( 2, 10, 200 )
!        ( 3, 10, 200 )
!        ( 4, 10, 200 )
!        ( 1, 20, 200 )
!        ( 2, 20, 200 )
!        ( 3, 20, 200 )
!        ( 4, 20, 200 )
!        ( 1, 30, 200 )
!        ( 2, 30, 200 )
!        ( 3, 30, 200 )
!        ( 4, 30, 200 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) FACTOR_INDEX, the index of the factor being
!    processed.  The first factor processed must be factor 1!
!
!    integer ( kind = 4 ) FACTOR_ORDER, the order of the factor.
!
!    real ( kind = 8 ) FACTOR_VALUE(FACTOR_ORDER), the factor values
!    for factor FACTOR_INDEX.
!
!    integer ( kind = 4 ) FACTOR_NUM, the number of factors.
!
!    integer ( kind = 4 ) POINT_NUM, the number of elements in the
!    direct product.
!
!    real ( kind = 8 ) X(FACTOR_NUM,POINT_NUM), the elements of
!    the direct product, which are built up gradually.
!
!  Output:
!
!    real ( kind = 8 ) X(FACTOR_NUM,POINT_NUM), the updated direct product.
!
!  Local:
!
!    integer ( kind = 4 ) START, the first location of a block of 
!    values to set.
!
!    integer ( kind = 4 ) CONTIG, the number of consecutive values 
!    to set.
!
!    integer ( kind = 4 ) SKIP, the distance from the current value 
!    of START to the next location of a block of values to set.
!
!    integer ( kind = 4 ) REP, the number of blocks of values to set.
!
  implicit none

  integer ( kind = 4 ) factor_num
  integer ( kind = 4 ) factor_order
  integer ( kind = 4 ) point_num

  integer ( kind = 4 ), save :: contig
  integer ( kind = 4 ) factor_index
  real ( kind = 8 ) factor_value(factor_order)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ), save :: rep
  integer ( kind = 4 ), save :: skip
  integer ( kind = 4 ) start
  real ( kind = 8 ) x(factor_num,point_num)

  if ( factor_index == 1 ) then
    contig = 1
    skip = 1
    rep = point_num
    x(1:factor_num,1:point_num) = 0.0D+00
  end if

  rep = rep / factor_order
  skip = skip * factor_order

  do j = 1, factor_order

    start = 1 + ( j - 1 ) * contig

    do k = 1, rep
      x(factor_index,start:start+contig-1) = factor_value(j)
      start = start + skip
    end do

  end do

  contig = contig * factor_order

  return
end
subroutine r8vec_direct_product2 ( factor_index, factor_order, factor_value, &
  factor_num, point_num, w )

!*****************************************************************************80
!
!! R8VEC_DIRECT_PRODUCT2 creates a direct product of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    To explain what is going on here, suppose we had to construct
!    a multidimensional quadrature rule as the product of K rules
!    for 1D quadrature.
!
!    The product rule will be represented as a list of points and weights.
!
!    The J-th item in the product rule will be associated with
!      item J1 of 1D rule 1,
!      item J2 of 1D rule 2,
!      ...,
!      item JK of 1D rule K.
!
!    In particular,
!      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
!    and
!      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)
!
!    So we can construct the quadrature rule if we can properly
!    distribute the information in the 1D quadrature rules.
!
!    This routine carries out the task involving the weights W.
!
!    Another way to do this would be to compute, one by one, the
!    set of all possible indices (J1,J2,...,JK), and then index
!    the appropriate information.  An advantage of the method shown
!    here is that you can process the K-th set of information and
!    then discard it.
!
!  Example:
!
!    Rule 1:
!      Order = 4
!      W(1:4) = ( 2, 3, 5, 7 )
!
!    Rule 2:
!      Order = 3
!      W(1:3) = ( 11, 13, 17 )
!
!    Rule 3:
!      Order = 2
!      W(1:2) = ( 19, 23 )
!
!    Product Rule:
!      Order = 24
!      W(1:24) =
!        ( 2 * 11 * 19 )
!        ( 3 * 11 * 19 )
!        ( 4 * 11 * 19 )
!        ( 7 * 11 * 19 )
!        ( 2 * 13 * 19 )
!        ( 3 * 13 * 19 )
!        ( 5 * 13 * 19 )
!        ( 7 * 13 * 19 )
!        ( 2 * 17 * 19 )
!        ( 3 * 17 * 19 )
!        ( 5 * 17 * 19 )
!        ( 7 * 17 * 19 )
!        ( 2 * 11 * 23 )
!        ( 3 * 11 * 23 )
!        ( 5 * 11 * 23 )
!        ( 7 * 11 * 23 )
!        ( 2 * 13 * 23 )
!        ( 3 * 13 * 23 )
!        ( 5 * 13 * 23 )
!        ( 7 * 13 * 23 )
!        ( 2 * 17 * 23 )
!        ( 3 * 17 * 23 )
!        ( 5 * 17 * 23 )
!        ( 7 * 17 * 23 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) FACTOR_INDEX, the index of the factor being
!    processed.  The first factor processed must be factor 1!
!
!    integer ( kind = 4 ) FACTOR_ORDER, the order of the factor.
!
!    real ( kind = 8 ) FACTOR_VALUE(FACTOR_ORDER), the factor values
!    for factor FACTOR_INDEX.
!
!    integer ( kind = 4 ) FACTOR_NUM, the number of factors.
!
!    integer ( kind = 4 ) POINT_NUM, the number of elements in the
!    direct product.
!
!    real ( kind = 8 ) W(POINT_NUM), the elements of the
!    direct product, which are built up gradually.
!
!  Output:
!
!    real ( kind = 8 ) W(POINT_NUM), the updated direct product.
!
!  Local:
!
!    integer ( kind = 4 ) START, the first location of a block of values
!    to set.
!
!    integer ( kind = 4 ) CONTIG, the number of consecutive values
!    to set.
!
!    integer ( kind = 4 ) SKIP, the distance from the current value 
!    of START to the next location of a block of values to set.
!
!    integer ( kind = 4 ) REP, the number of blocks of values to set.
!
  implicit none

  integer ( kind = 4 ) factor_num
  integer ( kind = 4 ) factor_order
  integer ( kind = 4 ) point_num

  integer ( kind = 4 ), save :: contig
  integer ( kind = 4 ) factor_index
  real ( kind = 8 ) factor_value(factor_order)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ), save :: rep
  integer ( kind = 4 ), save :: skip
  integer ( kind = 4 ) start
  real ( kind = 8 ) w(point_num)

  call i4_fake_use ( factor_num )

  if ( factor_index == 1 ) then
    contig = 1
    skip = 1
    rep = point_num
    w(1:point_num) = 1.0D+00
  end if

  rep = rep / factor_order
  skip = skip * factor_order

  do j = 1, factor_order

    start = 1 + ( j - 1 ) * contig

    do k = 1, rep
      w(start:start+contig-1) = w(start:start+contig-1) * factor_value(j)
      start = start + skip
    end do

  end do

  contig = contig * factor_order

  return
end
function r8vec_distance ( dim_num, v1, v2 )

!*****************************************************************************80
!
!! R8VEC_DISTANCE returns the Euclidean distance between two R8VEC's.
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
!    11 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) DIM_NUM, the spatial dimension.
!
!    real ( kind = 8 ) V1(DIM_NUM), V2(DIM_NUM), the vectors.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_DISTANCE, the Euclidean distance
!    between the vectors.
!
  implicit none

  integer ( kind = 4 ) dim_num

  real ( kind = 8 ) r8vec_distance
  real ( kind = 8 ) v1(dim_num)
  real ( kind = 8 ) v2(dim_num)

  r8vec_distance = sqrt ( sum ( ( v1(1:dim_num) - v2(1:dim_num) ) ** 2 ) )

  return
end
function r8vec_dot_product ( n, v1, v2 )

!*****************************************************************************80
!
!! R8VEC_DOT_PRODUCT finds the dot product of a pair of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In FORTRAN90, the system routine DOT_PRODUCT should be called
!    directly.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2008
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of the vectors.
!
!    real ( kind = 8 ) V1(N), V2(N), the vectors.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_DOT_PRODUCT, the dot product.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) r8vec_dot_product
  real ( kind = 8 ) v1(n)
  real ( kind = 8 ) v2(n)

  r8vec_dot_product = dot_product ( v1(1:n), v2(1:n) )

  return
end
function r8vec_dot_product_affine ( n, v0, v1, v2 )

!*****************************************************************************80
!
!! R8VEC_DOT_PRODUCT_AFFINE computes the affine dot product V1-V0 * V2-V0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the spatial dimension.
!
!    real ( kind = 8 ) V0(N), the base vector.
!
!    real ( kind = 8 ) V1(N), V2(N), the vectors.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_DOT_PRODUCT_AFFINE, the dot product.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) r8vec_dot_product_affine
  real ( kind = 8 ) v0(n)
  real ( kind = 8 ) v1(n)
  real ( kind = 8 ) v2(n)

  r8vec_dot_product_affine = dot_product ( &
    v1(1:n) - v0(1:n),  &
    v2(1:n) - v0(1:n) )

  return
end
function r8vec_entropy ( n, x )

!*****************************************************************************80
!
!! R8VEC_ENTROPY computes the entropy of an R8VEC.
!
!  Discussion:
!
!    Typically, the entries represent probabilities, and must sum to 1.
!    For this function, the only requirement is that the entries be nonnegative.
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries.
!
!    real ( kind = 8 ) X(N), the vector.
!    Each entry must be nonnegative.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_ENTROPY, the entropy of the
!    normalized vector.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) r8_log_2
  real ( kind = 8 ) r8vec_entropy
  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_sum
  real ( kind = 8 ) xi

  if ( any ( x(1:n) < 0.0D+00 ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_ENTROPY - Fatal error!'
    write ( *, '(a)' ) '  Some entries are negative.'
    stop 1
  end if

  x_sum = sum ( x(1:n) )

  if ( x_sum == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_ENTROPY - Fatal error!'
    write ( *, '(a)' ) '  Entries sum to 0.'
    stop 1
  end if

  value = 0.0D+00
  do i = 1, n
    if ( 0.0D+00 < x(i) ) then
      xi = x(i) / x_sum
      value = value - r8_log_2 ( xi ) * xi
    end if
  end do

  r8vec_entropy = value

  return
end
function r8vec_eq ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_EQ is true if two R8VECs are equal.
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
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    real ( kind = 8 ) A1(N), A2(N), two vectors to compare.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_EQ, is TRUE if every pair of elements 
!    A1(I) and A2(I) are equal, and FALSE otherwise.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  logical ( kind = 4 ) r8vec_eq

  r8vec_eq = ( all ( a1(1:n) == a2(1:n) ) )

  return
end
subroutine r8vec_even ( n, alo, ahi, a )

!*****************************************************************************80
!
!! R8VEC_EVEN returns an R8VEC of evenly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If N is 1, then the midpoint is returned.
!
!    Otherwise, the two endpoints are returned, and N-2 evenly
!    spaced points between them.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of values.
!
!    real ( kind = 8 ) ALO, AHI, the low and high values.
!
!  Output:
!
!    real ( kind = 8 ) A(N), N evenly spaced values.
!    Normally, A(1) = ALO and A(N) = AHI.
!    However, if N = 1, then A(1) = 0.5*(ALO+AHI).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) ahi
  real ( kind = 8 ) alo
  integer ( kind = 4 ) i

  if ( n == 1 ) then

    a(1) = 0.5D+00 * ( alo + ahi )

  else

    do i = 1, n
      a(i) = ( real ( n - i,     kind = 8 ) * alo   &
             + real (     i - 1, kind = 8 ) * ahi ) &
             / real ( n     - 1, kind = 8 )
    end do

  end if

  return
end
subroutine r8vec_even_select ( n, xlo, xhi, ival, xval )

!*****************************************************************************80
!
!! R8VEC_EVEN_SELECT returns the I-th of N evenly spaced values in [ XLO, XHI ].
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    XVAL = ( (N-IVAL) * XLO + (IVAL-1) * XHI ) / real ( N - 1 )
!
!    Unless N = 1, X(1) = XLO and X(N) = XHI.
!
!    If N = 1, then X(1) = 0.5*(XLO+XHI).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of values.
!
!    real ( kind = 8 ) XLO, XHI, the low and high values.
!
!    integer ( kind = 4 ) IVAL, the index of the desired point.
!    IVAL is normally between 1 and N, but may be any integer value.
!
!  Output:
!
!    real ( kind = 8 ) XVAL, the IVAL-th of N evenly spaced values
!    between XLO and XHI.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ival
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xval

  if ( n == 1 ) then

    xval = 0.5D+00 * ( xlo + xhi )

  else

    xval = ( real ( n - ival,     kind = 8 ) * xlo   &
           + real (     ival - 1, kind = 8 ) * xhi ) &
           / real ( n        - 1, kind = 8 )

  end if

  return
end
subroutine r8vec_even2 ( maxval, nfill, nold, xold, nval, xval )

!*****************************************************************************80
!
!! R8VEC_EVEN2 linearly interpolates new numbers into an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The number of values created between two old values can vary from
!    one pair of values to the next.
!
!    The interpolated values are evenly spaced.
!
!    This routine is a generalization of R8VEC_EVEN.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) MAXVAL, the size of the XVAL array, as declared
!    by the user.  MAXVAL must be large enough to hold the NVAL values computed
!    by this routine.  In other words, MAXVAL must be at least equal to
!    NOLD + SUM (1 <= I <= NOLD-1) NFILL(I).
!
!    integer ( kind = 4 ) NFILL(NOLD-1), the number of values
!    to be interpolated between XOLD(I) and XOLD(I+1).
!    NFILL(I) does not count the endpoints.  Thus, if
!    NFILL(I) is 1, there will be one new point generated
!    between XOLD(I) and XOLD(I+1).
!    NFILL(I) must be nonnegative.
!
!    integer ( kind = 4 ) NOLD, the number of values XOLD,
!    between which extra values are to be interpolated.
!
!    real ( kind = 8 ) XOLD(NOLD), the original vector of numbers
!    between which new values are to be interpolated.
!
!  Output:
!
!    integer ( kind = 4 ) NVAL, the number of values computed
!    in the XVAL array.
!    NVAL = NOLD + SUM ( 1 <= I <= NOLD-1 ) NFILL(I)
!
!    real ( kind = 8 ) XVAL(MAXVAL).  On XVAL contains the
!    NOLD values of XOLD, as well as the interpolated
!    values, making a total of NVAL values.
!
  implicit none

  integer ( kind = 4 ) maxval
  integer ( kind = 4 ) nold

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nadd
  integer ( kind = 4 ) nfill(nold-1)
  integer ( kind = 4 ) nval
  real ( kind = 8 ) xold(nold)
  real ( kind = 8 ) xval(maxval)

  nval = 1

  do i = 1, nold - 1

    if ( nfill(i) < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_EVEN2 - Fatal error!'
      write ( *, '(a,i8)' ) '  NFILL(I) is negative for I = ', i
      write ( *, '(a,i8)' ) '  NFILL(I) = ', nfill(i)
      stop 1
    end if

    if ( maxval < nval + nfill(i) + 1 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_EVEN2 - Fatal error!'
      write ( *, '(a)' ) '  MAXVAL is not large enough.  '
      write ( *, '(a,i8)' ) '  MAXVAL = ', maxval
      write ( *, '(a)' ) '  which is exceeded by storage requirements'
      write ( *, '(a,i8)' ) '  for interpolating in interval ', i
      stop 1
    end if

    nadd = nfill(i) + 2

    do j = 1, nadd
      xval(nval+j-1) = ( real ( nadd - j,     kind = 8 ) * xold(i)   &
                       + real (        j - 1, kind = 8 ) * xold(i+1) ) &
                       / real ( nadd     - 1, kind = 8 )
    end do

    nval = nval + nfill(i) + 1

  end do

  return
end
subroutine r8vec_even2_select ( n, xlo, xhi, ival, xval )

!*****************************************************************************80
!
!! R8VEC_EVEN2_SELECT returns the I-th of N evenly spaced midpoint values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This function returns the I-th of N evenly spaced midpoints of N
!    equal subintervals of [XLO,XHI].
!
!    XVAL = ( ( 2 * N - 2 * IVAL + 1 ) * XLO 
!           + (         2 * IVAL - 1 ) * XHI ) 
!           / ( 2 * N                )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 July 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of values.
!
!    real ( kind = 8 ) XLO, XHI, the low and high values.
!
!    integer ( kind = 4 ) IVAL, the index of the desired point.
!    IVAL is normally between 1 and N, but may be any integer value.
!
!  Output:
!
!    real ( kind = 8 ) XVAL, the IVAL-th of N evenly spaced midpoints
!    between XLO and XHI.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ival
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xval

  xval = ( real ( 2 * n - 2 * ival + 1, kind = 8 ) * xlo   &
         + real (         2 * ival - 1, kind = 8 ) * xhi ) &
         / real ( 2 * n, kind = 8 )

  return
end
subroutine r8vec_even3 ( nold, nval, xold, xval )

!*****************************************************************************80
!
!! R8VEC_EVEN3 evenly interpolates new data into an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This routine accepts a short vector of numbers, and returns a longer
!    vector of numbers, created by interpolating new values between
!    the given values.
!
!    Between any two original values, new values are evenly interpolated.
!
!    Over the whole vector, the new numbers are interpolated in
!    such a way as to try to minimize the largest distance interval size.
!
!    The algorithm employed is not "perfect".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) NOLD, the number of values XOLD, between
!    which extra values are to be interpolated.
!
!    integer ( kind = 4 ) NVAL, the number of values to be computed
!    in the XVAL array.  NVAL should be at least NOLD.
!
!    real ( kind = 8 ) XOLD(NOLD), the original vector of numbers
!    between which new values are to be interpolated.
!
!  Output:
!
!    real ( kind = 8 ) XVAL(NVAL).  On XVAL contains the
!    NOLD values of XOLD, as well as interpolated
!    values, making a total of NVAL values.
!
  implicit none

  integer ( kind = 4 ) nval
  integer ( kind = 4 ) nold

  real ( kind = 8 ) density
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ival
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nmaybe
  integer ( kind = 4 ) npts
  integer ( kind = 4 ) ntemp
  integer ( kind = 4 ) ntot
  real ( kind = 8 ) xlen
  real ( kind = 8 ) xleni
  real ( kind = 8 ) xlentot
  real ( kind = 8 ) xold(nold)
  real ( kind = 8 ) xval(nval)

  xlen = 0.0D+00
  do i = 1, nold - 1
    xlen = xlen + abs ( xold(i+1) - xold(i) )
  end do

  ntemp = nval - nold

  density = real ( ntemp, kind = 8 ) / xlen

  ival = 1
  ntot = 0
  xlentot = 0.0D+00

  do i = 1, nold - 1

    xleni = abs ( xold(i+1) - xold(i) )
    npts = int ( density * xleni )
    ntot = ntot + npts
!
!  Determine if we have enough left-over density that it should
!  be changed into a point.  A better algorithm would agonize
!  more over where that point should go.
!
    xlentot = xlentot + xleni
    nmaybe = nint ( xlentot * density )

    if ( ntot < nmaybe ) then
      npts = npts + nmaybe - ntot
      ntot = nmaybe
    end if

    do j = 1, npts + 2
      xval(ival+j-1) = ( real ( npts+2 - j,     kind = 8 ) * xold(i)   &
                       + real (          j - 1, kind = 8 ) * xold(i+1) ) &
                       / real ( npts+2     - 1, kind = 8 )
    end do

    ival = ival + npts + 1

  end do

  return
end
subroutine r8vec_expand_linear ( n, x, fat, xfat )

!*****************************************************************************80
!
!! R8VEC_EXPAND_LINEAR linearly interpolates new data into an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This routine copies the old data, and inserts NFAT new values
!    between each pair of old data values.  This would be one way to
!    determine places to evenly sample a curve, given the (unevenly
!    spaced) points at which it was interpolated.
!
!  Example:
!
!    N = 3
!    NFAT = 2
!
!    X(1:N)        = (/ 0.0,           6.0,             7.0 /)
!    XFAT(1:2*3+1) = (/ 0.0, 2.0, 4.0, 6.0, 6.33, 6.66, 7.0 /)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of input data values.
!
!    real ( kind = 8 ) X(N), the original data.
!
!    integer ( kind = 4 ) FAT, the number of data values to interpolate
!    between each pair of original data values.
!
!  Output:
!
!    real ( kind = 8 ) XFAT((N-1)*(FAT+1)+1), the "fattened" data.
!
  implicit none

  integer ( kind = 4 ) fat
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xfat((n-1)*(fat+1)+1)

  k = 0

  do i = 1, n - 1

    k = k + 1
    xfat(k) = x(i)

    do j = 1, fat
      k = k + 1
      xfat(k) = ( real ( fat - j + 1, kind = 8 ) * x(i)     &
                + real (       j,     kind = 8 ) * x(i+1) ) &
                / real ( fat     + 1, kind = 8 )
    end do

  end do

  k = k + 1
  xfat(k) = x(n)

  return
end
subroutine r8vec_expand_linear2 ( n, x, before, fat, after, xfat )

!*****************************************************************************80
!
!! R8VEC_EXPAND_LINEAR2 linearly interpolates new data into an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This routine starts with a vector of data.
!
!    The intent is to "fatten" the data, that is, to insert more points
!    between successive values of the original data.
!
!    There will also be extra points placed BEFORE the first original
!    value and AFTER that last original value.
!
!    The "fattened" data is equally spaced between the original points.
!
!    The BEFORE data uses the spacing of the first original interval,
!    and the AFTER data uses the spacing of the last original interval.
!
!  Example:
!
!    N = 3
!    BEFORE = 3
!    FAT = 2
!    AFTER = 1
!
!    X    = (/                   0.0,           6.0,             7.0       /)
!    XFAT = (/ -6.0, -4.0, -2.0, 0.0, 2.0, 4.0, 6.0, 6.33, 6.66, 7.0, 7.66 /)
!            3 "BEFORE's"        Old  2 "FATS"  Old    2 "FATS"  Old  1 "AFTER"
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
!  Input:
!
!    integer ( kind = 4 ) N, the number of input data values.
!    N must be at least 2.
!
!    real ( kind = 8 ) X(N), the original data.
!
!    integer ( kind = 4 ) BEFORE, the number of "before" values.
!
!    integer ( kind = 4 ) FAT, the number of data values to interpolate
!    between each pair of original data values.
!
!    integer ( kind = 4 ) AFTER, the number of "after" values.
!
!  Output:
!
!    real ( kind = 8 ) XFAT(BEFORE+(N-1)*(FAT+1)+1+AFTER), the
!    "fattened" data.
!
  implicit none

  integer ( kind = 4 ) after
  integer ( kind = 4 ) before
  integer ( kind = 4 ) fat
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xfat(before+(n-1)*(fat+1)+1+after)

  k = 0
!
!  Points BEFORE.
!
  do j = 1 - before + fat, fat
    k = k + 1
    xfat(k) = ( real ( fat - j + 1, kind = 8 ) * ( x(1) - ( x(2) - x(1) ) ) &
              + real (       j,     kind = 8 ) *   x(1)          ) &
              / real ( fat     + 1, kind = 8 )
  end do
!
!  Original points and FAT points.
!
  do i = 1, n - 1

    k = k + 1
    xfat(k) = x(i)

    do j = 1, fat
      k = k + 1
      xfat(k) = ( real ( fat - j + 1, kind = 8 ) * x(i)     &
                + real (       j,     kind = 8 ) * x(i+1) ) &
                / real ( fat     + 1, kind = 8 )
    end do

  end do

  k = k + 1
  xfat(k) = x(n)
!
!  Points AFTER.
!
  do j = 1, after
    k = k + 1
    xfat(k) = ( real ( fat - j + 1, kind = 8 ) * x(n)     &
              + real (       j,     kind = 8 ) &
              * ( x(n) + ( x(n) - x(n-1) ) ) ) &
              / real ( fat     + 1, kind = 8 )
  end do

  return
end
subroutine r8vec_fill ( n, value, x )

!*****************************************************************************80
!
!! R8VEC_FILL sets all entries of an R8VEC to a given value.
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
!    03 December 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the vector.
!
!    real ( kind = 8 ) VALUE, the value.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) value
  real ( kind = 8 ) x(n)

  x(1:n) = value

  return
end
subroutine r8vec_first_index ( n, a, tol, first_index )

!*****************************************************************************80
!
!! R8VEC_FIRST_INDEX indexes the first occurrence of values in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    For element A(I) of the vector, FIRST_INDEX(I) is the index in A of
!    the first occurrence of the value A(I).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2008
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!    real ( kind = 8 ) A(N), the array.
!
!    real ( kind = 8 ) TOL, a tolerance for equality.
!
!  Output:
!
!    integer ( kind = 4 ) FIRST_INDEX(N), the first occurrence index.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) first_index(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) tol

  first_index(1:n) = -1

  do i = 1, n

    if ( first_index(i) == -1 ) then

      first_index(i) = i

      do j = i + 1, n
        if ( abs ( a(i) - a(j) ) <= tol ) then
          first_index(j) = i
        end if
      end do

    end if

  end do

  return
end
subroutine r8vec_floor ( n, r8vec, floorvec )

!*****************************************************************************80
!
!! R8VEC_FLOOR rounds "down" (towards -oo) entries of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Example:
!
!    R8    Value
!
!   -1.1  -2
!   -1.0  -1
!   -0.9  -1
!    0.0   0
!    5.0   5
!    5.1   5
!    5.9   5
!    6.0   6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries.
!
!    real ( kind = 8 ) R8VEC(N), the values to be rounded down.
!
!  Output:
!
!    integer ( kind = 4 ) FLOORVEC(N), the rounded value.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) floorvec(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec(n)
  integer ( kind = 4 ) value

  do i = 1, n

    value = int ( r8vec(i) )

    if ( r8vec(i) < real ( value, kind = 8 ) ) then
      value = value - 1
    end if

    floorvec(i) = value

  end do

  return
end
subroutine r8vec_frac ( n, a, k, frac )

!*****************************************************************************80
!
!! R8VEC_FRAC searches for the K-th smallest entry in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Hoare's algorithm is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!    real ( kind = 8 ) A(N), the array to search.
!
!    integer ( kind = 4 ) K, the fractile to be sought.  If K = 1, the
!    minimum entry is sought.  If K = N, the maximum is sought.  Other values
!    of K search for the entry which is K-th in size.  K must be at
!    least 1, and no greater than N.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the elements have been somewhat rearranged.
!
!    real ( kind = 8 ) FRAC, the value of the K-th fractile of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) frac
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iryt
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) left
  real ( kind = 8 ) temp
  real ( kind = 8 ) x

  if ( n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_FRAC - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal nonpositive value of N = ', n
    stop 1
  end if

  if ( k <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_FRAC - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal nonpositive value of K = ', k
    stop 1
  end if

  if ( n < k ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_FRAC - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal N < K, K = ', k
    stop 1
  end if

  left = 1
  iryt = n

  do

    if ( iryt <= left ) then
      frac = a(k)
      exit
    end if

    x = a(k)
    i = left
    j = iryt

    do

      if ( j < i ) then
        if ( j < k ) then
          left = i
        end if
        if ( k < i ) then
          iryt = j
        end if
        exit
      end if
!
!  Find I so that X <= A(I).
!
      do while ( a(i) < x )
        i = i + 1
      end do
!
!  Find J so that A(J) <= X.
!
      do while ( x < a(j) )
        j = j - 1
      end do

      if ( i <= j ) then

        temp = a(i)
        a(i) = a(j)
        a(j) = temp

        i = i + 1
        j = j - 1
      end if

    end do

  end do

  return
end
subroutine r8vec_fraction ( n, x, fraction )

!*****************************************************************************80
!
!! R8VEC_FRACTION returns the fraction parts of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If we regard a real number as
!
!      R8 = SIGN * ( WHOLE + FRACTION )
!
!    where
!
!      SIGN is +1 or -1,
!      WHOLE is a nonnegative integer
!      FRACTION is a nonnegative real number strictly less than 1,
!
!    then this routine returns the value of FRACTION.
!
!  Example:
!
!     R8    R8_FRACTION
!
!    0.00      0.00
!    1.01      0.01
!    2.02      0.02
!   19.73      0.73
!   -4.34      0.34
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of arguments.
!
!    real ( kind = 8 ) X(N), the arguments.
!
!  Output:
!
!    real ( kind = 8 ) FRACTION(N), the fraction parts.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fraction(n)
  real ( kind = 8 ) x(n)

  fraction(1:n) = abs ( x(1:n) ) - real ( int ( abs ( x(1:n) ) ), kind = 8 )

  return
end
function r8vec_gt ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_GT == ( A1 > A2 ) for R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The comparison is lexicographic.
!
!    A1 > A2  <=>                              A1(1) > A2(1) or
!                 ( A1(1)     == A2(1)     and A1(2) > A2(2) ) or
!                 ...
!                 ( A1(1:N-1) == A2(1:N-1) and A1(N) > A2(N)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of the vectors.
!
!    real ( kind = 8 ) A1(N), A2(N), the vectors to be compared.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_GT, is TRUE if and only if A1 > A2.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) i
  logical ( kind = 4 ) r8vec_gt

  r8vec_gt = .false.

  do i = 1, n

    if ( a2(i) < a1(i) ) then
      r8vec_gt = .true.
      exit
    else if ( a1(i) < a2(i) ) then
      r8vec_gt = .false.
      exit
    end if

  end do

  return
end
subroutine r8vec_heap_a ( n, a )

!*****************************************************************************80
!
!! R8VEC_HEAP_A reorders an R8VEC into an ascending heap.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An ascending heap is an array A with the property that, for every index J,
!    A(J) <= A(2*J) and A(J) <= A(2*J+1), (as long as the indices
!    2*J and 2*J+1 are legal).
!
!                  A(1)
!                /      \
!            A(2)         A(3)
!          /     \        /  \
!      A(4)       A(5)  A(6) A(7)
!      /  \       /   \
!    A(8) A(9) A(10) A(11)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 July 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the input array.
!
!    real ( kind = 8 ) A(N), an unsorted array.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the heap-sorted array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifree
  real ( kind = 8 ) key
  integer ( kind = 4 ) m
!
!  Only nodes N/2 down to 1 can be "parent" nodes.
!
  do i = n / 2, 1, -1
!
!  Copy the value out of the parent node.
!  Position IFREE is now "open".
!
    key = a(i)
    ifree = i

    do
!
!  Positions 2*IFREE and 2*IFREE + 1 are the descendants of position
!  IFREE.  (One or both may not exist because they exceed N.)
!
      m = 2 * ifree
!
!  Does the first position exist?
!
      if ( n < m ) then
        exit
      end if
!
!  Does the second position exist?
!
      if ( m + 1 <= n ) then
!
!  If both positions exist, take the smaller of the two values,
!  and update M if necessary.
!
        if ( a(m+1) < a(m) ) then
          m = m + 1
        end if

      end if
!
!  If the small descendant is smaller than KEY, move it up,
!  and update IFREE, the location of the free position, and
!  consider the descendants of THIS position.
!
      if ( key <= a(m) ) then
        exit
      end if

      a(ifree) = a(m)
      ifree = m

    end do
!
!  Once there is no more shifting to do, KEY moves into the free spot.
!
    a(ifree) = key

  end do

  return
end
subroutine r8vec_heap_d ( n, a )

!*****************************************************************************80
!
!! R8VEC_HEAP_D reorders an R8VEC into an descending heap.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    A descending heap is an array A with the property that, for every index J,
!    A(J) >= A(2*J) and A(J) >= A(2*J+1), (as long as the indices
!    2*J and 2*J+1 are legal).
!
!                  A(1)
!                /      \
!            A(2)         A(3)
!          /     \        /  \
!      A(4)       A(5)  A(6) A(7)
!      /  \       /   \
!    A(8) A(9) A(10) A(11)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 July 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the input array.
!
!    real ( kind = 8 ) A(N), the array to be sorted.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the heap-sorted array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifree
  real ( kind = 8 ) key
  integer ( kind = 4 ) m
!
!  Only nodes N/2 down to 1 can be "parent" nodes.
!
  do i = n / 2, 1, -1
!
!  Copy the value out of the parent node.
!  Position IFREE is now "open".
!
    key = a(i)
    ifree = i

    do
!
!  Positions 2*IFREE and 2*IFREE + 1 are the descendants of position
!  IFREE.  (One or both may not exist because they exceed N.)
!
      m = 2 * ifree
!
!  Does the first position exist?
!
      if ( n < m ) then
        exit
      end if
!
!  Does the second position exist?
!
      if ( m + 1 <= n ) then
!
!  If both positions exist, take the larger of the two values,
!  and update M if necessary.
!
        if ( a(m) < a(m+1) ) then
          m = m + 1
        end if

      end if
!
!  If the large descendant is larger than KEY, move it up,
!  and update IFREE, the location of the free position, and
!  consider the descendants of THIS position.
!
      if ( a(m) <= key ) then
        exit
      end if

      a(ifree) = a(m)
      ifree = m

    end do
!
!  Once there is no more shifting to do, KEY moves into the free spot IFREE.
!
    a(ifree) = key

  end do

  return
end
subroutine r8vec_heap_d_extract ( n, a, value )

!*****************************************************************************80
!
!! R8VEC_HEAP_D_EXTRACT: extract maximum from a heap descending sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In other words, the routine finds the maximum value in the
!    heap, returns that value to the user, deletes that value from
!    the heap, and restores the heap to its proper form.
!
!    This is one of three functions needed to model a priority queue.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Cormen, Charles Leiserson, Ronald Rivest,
!    Introduction to Algorithms,
!    MIT Press, 2001,
!    ISBN: 0262032937,
!    LC: QA76.C662.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements in the heap.
!
!    real ( kind = 8 ) A(N), the heap.
!
!  Output:
!
!    integer ( kind = 4 ) N, the updated number of items in the heap.
!
!    real ( kind = 8 ) A(N), the updated heap.
!
!    real ( kind = 8 ) VALUE, the item of maximum value, which has
!    been removed from the heap.
!
  implicit none

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) n
  real ( kind = 8 ) value

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_HEAP_D_EXTRACT - Fatal error!'
    write ( *, '(a)' ) '  The heap is empty.'
    stop 1
  end if
!
!  Get the maximum value.
!
  value = a(1)

  if ( n == 1 ) then
    n = 0
    return
  end if
!
!  Shift the last value down.
!
  a(1) = a(n)
!
!  Restore the heap structure.
!
  n = n - 1
  call r8vec_sort_heap_d ( n, a )

  return
end
subroutine r8vec_heap_d_insert ( n, a, value )

!*****************************************************************************80
!
!! R8VEC_HEAP_D_INSERT inserts a value into a heap descending sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This is one of three functions needed to model a priority queue.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Cormen, Charles Leiserson, Ronald Rivest,
!    Introduction to Algorithms,
!    MIT Press, 2001,
!    ISBN: 0262032937,
!    LC: QA76.C662.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items in the heap.
!
!    real ( kind = 8 ) A(N), the heap.
!
!    real ( kind = 8 ) VALUE, the value to be inserted.
!
!  Output:
!
!    integer ( kind = 4 ) N, the updated number of items in the heap.
!
!    real ( kind = 8 ) A(N), the updated heap.
!
  implicit none

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) parent
  real ( kind = 8 ) value

  n = n + 1
  i = n

  do while ( 1 < i )

    parent = i / 2

    if ( value <= a(parent) ) then
      exit
    end if

    a(i) = a(parent)
    i = parent

  end do

  a(i) = value

  return
end
subroutine r8vec_heap_d_max ( n, a, value )

!*****************************************************************************80
!
!! R8VEC_HEAP_D_MAX returns the maximum value in a heap descending sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This is one of three functions needed to model a priority queue.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Cormen, Charles Leiserson, Ronald Rivest,
!    Introduction to Algorithms,
!    MIT Press, 2001,
!    ISBN: 0262032937,
!    LC: QA76.C662.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items in the heap.
!
!    real ( kind = 8 ) A(N), the heap.
!
!  Output:
!
!    real ( kind = 8 ) VALUE, the maximum value in the heap.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) value

  value = a(1)

  return
end
subroutine r8vec_histogram ( n, a, a_lo, a_hi, histo_num, histo_gram )

!*****************************************************************************80
!
!! R8VEC_HISTOGRAM histograms an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Values between A_LO and A_HI will be histogrammed into the bins
!    1 through HISTO_NUM.  Values below A_LO are counted in bin 0,
!    and values greater than A_HI are counted in bin HISTO_NUM+1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!    real ( kind = 8 ) A(N), the array to examine.
!
!    real ( kind = 8 ) A_LO, A_HI, the lowest and highest
!    values to be histogrammed.  These values will also define the bins.
!
!    integer ( kind = 4 ) HISTO_NUM, the number of bins to use.
!
!  Output:
!
!    integer ( kind = 4 ) HISTO_GRAM(0:HISTO_NUM+1), contains the
!    number of entries of A in each bin.
!
  implicit none

  integer ( kind = 4 ) histo_num
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_hi
  real ( kind = 8 ) a_lo
  real ( kind = 8 ) delta
  integer ( kind = 4 ) histo_gram(0:histo_num+1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  histo_gram(0:histo_num+1) = 0

  delta = ( a_hi - a_lo ) / real ( 2 * histo_num, kind = 8 )

  do i = 1, n

    if ( a(i) < a_lo ) then

      histo_gram(0) = histo_gram(0) + 1

    else if ( a(i) <= a_hi ) then

      j = nint ( &
        ( ( a_hi -           delta - a(i)        ) &
        * real ( 1,         kind = 8 )   &
        + (      -           delta + a(i) - a_lo ) &
        * real ( histo_num, kind = 8 ) ) &
        / ( a_hi - 2.0D+00 * delta        - a_lo ) )

      histo_gram(j) = histo_gram(j) + 1

    else if ( a_hi < a(i) ) then

      histo_gram(histo_num+1) = histo_gram(histo_num+1) + 1

    end if

  end do

  return
end
subroutine r8vec_house_column ( n, a_vec, k, v )

!*****************************************************************************80
!
!! R8VEC_HOUSE_COLUMN defines a Householder premultiplier that "packs" a column.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine returns a vector V that defines a Householder
!    premultiplier matrix H(V) that zeros out the subdiagonal entries of
!    column K of the matrix A.
!
!       H(V) = I - 2 * v * v'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix A.
!
!    real ( kind = 8 ) A_VEC(N), a row or column of the matrix A.
!
!    integer ( kind = 4 ) K, the "special" entry in A_VEC.
!    The Householder matrix will zero out the entries after this.
!
!  Output:
!
!    real ( kind = 8 ) V(N), a vector of unit L2 norm which defines an
!    orthogonal Householder premultiplier matrix H with the property
!    that the K-th column of H*A is zero below the diagonal.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a_vec(n)
  integer ( kind = 4 ) k
  real ( kind = 8 ) s
  real ( kind = 8 ) v(n)

  v(1:n) = 0.0D+00

  if ( k < 1 .or. n <= k ) then
    return
  end if

  s = sqrt ( dot_product ( a_vec(k:n), a_vec(k:n) ) )

  if ( s == 0.0D+00 ) then
    return
  end if

  v(k) = a_vec(k) + sign ( s, a_vec(k) )
  v(k+1:n) = a_vec(k+1:n)
!
!  Normalize V.
!
  s = sqrt ( dot_product ( v(k:n), v(k:n) ) )

  v(k:n) = v(k:n) / s

  return
end
function r8vec_i4vec_dot_product ( n, r8vec, i4vec )

!*****************************************************************************80
!
!! R8VEC_I4VEC_DOT_PRODUCT finds the dot product of an R8VEC and an I4VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of the vectors.
!
!    real ( kind = 8 ) R8VEC(N), the first vector.
!
!    integer ( kind = 4 ) I4VEC(N), the second vector.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_I4VEC_DOT_PRODUCT, the dot product.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i4vec(n)
  real ( kind = 8 ) r8vec(n)
  real ( kind = 8 ) r8vec_i4vec_dot_product

  r8vec_i4vec_dot_product = dot_product ( r8vec(1:n), &
                                   real ( i4vec(1:n), kind = 8 ) )

  return
end
subroutine r8vec_identity_row ( n, i, a )

!*****************************************************************************80
!
!! R8VEC_IDENTITY_ROW returns the I-th row of the identity matrix.
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
!    28 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!    integer ( kind = 4 ) I, the entry to be set to 1.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i

  a(1:n) = 0.0D+00
  if ( 1 <= i .and. i <= n ) then
    a(i) = 1.0D+00
  end if

  return
end
subroutine r8vec_index_delete_all ( n, x, indx, xval )

!*****************************************************************************80
!
!! R8VEC_INDEX_DELETE_ALL deletes a value from an indexed sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Note that the value of N is adjusted because of the deletions!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the current list.
!
!    real ( kind = 8 ) X(N), the list.
!
!    integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    real ( kind = 8 ) XVAL, the value to be sought.
!
!  Output:
!
!    integer ( kind = 4 ) N, the updated size of the current list.
!
!    real ( kind = 8 ) X(N), the updated list.
!
!    integer ( kind = 4 ) INDX(N), the updated sort index of the list.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) equal1
  integer ( kind = 4 ) equal2
  integer ( kind = 4 ) get
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(*)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  integer ( kind = 4 ) put
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) xval

  if ( n < 1 ) then
    n = 0
    return
  end if

  call r8vec_index_search ( n, x, indx, xval, less, equal, more )

  if ( equal == 0 ) then
    return
  end if

  equal1 = equal

  do

    if ( equal1 <= 1 ) then
      exit
    end if

    if ( x(indx(equal1-1)) /= xval ) then
      exit
    end if

    equal1 = equal1 - 1

  end do

  equal2 = equal

  do

    if ( n <= equal2 ) then
      exit
    end if

    if ( x(indx(equal2+1)) /= xval ) then
      exit
    end if

    equal2 = equal2 + 1

  end do
!
!  Discard certain X values.
!
  put = 0

  do get = 1, n

    if ( x(get) /= xval ) then
      put = put + 1
      x(put) = x(get)
    end if

  end do

  x(put+1:n) = 0.0D+00
!
!  Adjust the INDX values.
!
  do equal = equal1, equal2
    do i = 1, n
      if ( indx(equal) < indx(i) ) then
        indx(i) = indx(i) - 1
      end if
    end do
  end do
!
!  Discard certain INDX values.
!
  indx(equal1:n+equal1-equal2-1) = indx(equal2+1:n)
  indx(n+equal1-equal2:n) = 0
!
!  Adjust N.
!
  n = put

  return
end
subroutine r8vec_index_delete_dupes ( n, x, indx, n2, x2, indx2 )

!*****************************************************************************80
!
!! R8VEC_INDEX_DELETE_DUPES deletes duplicates from an indexed sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The output quantities N2, X2, and INDX2 are computed from the
!    input quantities by sorting, and eliminating duplicates.
!
!    The output arrays should be dimensioned of size N, unless the user
!    knows in advance what the value of N2 will be.
!
!    The output arrays may be identified with the input arrays.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the input list.
!
!    real ( kind = 8 ) X(N), the list.
!
!    integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!  Output:
!
!    integer ( kind = 4 ) N2, the number of unique entries in X.
!
!    real ( kind = 8 ) X2(N2), a copy of the list which has
!    been sorted, and made unique.
!
!    integer ( kind = 4 ) INDX2(N2), the sort index of the new list.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indx2(n)
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) n3
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x2(n)
  real ( kind = 8 ) x3(n)

  i = 0
  n3 = 0

  do

    i = i + 1

    if ( n < i ) then
      exit
    end if

    if ( 1 < i ) then
      if ( x(indx(i)) == x3(n3) ) then
        cycle
      end if
    end if

    n3 = n3 + 1
    x3(n3) = x(indx(i))

  end do
!
!  Copy data into output arrays.
!
  n2 = n3
  x2(1:n2) = x3(1:n3)
  call i4vec_indicator1 ( n2, indx2 )

  return
end
subroutine r8vec_index_delete_one ( n, x, indx, xval, n2, x2, indx2 )

!*****************************************************************************80
!
!! R8VEC_INDEX_DELETE_ONE deletes one copy of a value from indexed sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If the value occurs in the list more than once, only one copy is deleted.
!
!    Note that the value of N is adjusted because of the deletions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the current list.
!
!    real ( kind = 8 ) X(N), the list.
!
!    integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    real ( kind = 8 ) XVAL, the value to be sought.
!
!  Output:
!
!    integer ( kind = 4 ) N2, the size of the current list.
!
!    real ( kind = 8 ) X2(N2), the list.
!
!    integer ( kind = 4 ) INDX2(N2), the sort index of the list.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indx2(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  integer ( kind = 4 ) n2
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x2(n)
  real ( kind = 8 ) xval

  if ( n < 1 ) then
    n2 = 0
    return
  end if

  n2 = n
  indx2(1:n2) = indx(1:n2)
  x2(1:n2) = x(1:n2)

  call r8vec_index_search ( n2, x2, indx2, xval, less, equal, more )

  if ( equal /= 0 ) then
    j = indx2(equal)
    x2(j:n2-1) = x2(j+1:n2)
    indx2(equal:n2-1) = indx2(equal+1:n2)
    do i = 1, n2-1
      if ( j < indx2(i) ) then
        indx2(i) = indx2(i) - 1
      end if
    end do
    n2 = n2 - 1
  end if

  return
end
subroutine r8vec_index_insert ( n, x, indx, xval )

!*****************************************************************************80
!
!! R8VEC_INDEX_INSERT inserts a value in an indexed sorted R8VEC.
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
!    11 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the current list.
!
!    real ( kind = 8 ) X(N), the list.
!
!    integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    real ( kind = 8 ) XVAL, the value to be sought.
!
!  Output:
!
!    integer ( kind = 4 ) N, the updated size of the current list.
!
!    real ( kind = 8 ) X(N), the updated list.
!
!    integer ( kind = 4 ) INDX(N), the updated sort index of the list.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) indx(*)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) xval

  if ( n <= 0 ) then
    n = 1
    x(1) = xval
    indx(1) = 1
    return
  end if

  call r8vec_index_search ( n, x, indx, xval, less, equal, more )

  x(n+1) = xval
  indx(n+1:more+1:-1) = indx(n:more:-1)
  indx(more) = n + 1
  n = n + 1

  return
end
subroutine r8vec_index_insert_unique ( n, x, indx, xval )

!*****************************************************************************80
!
!! R8VEC_INDEX_INSERT_UNIQUE inserts a unique value in an indexed sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If the value does not occur in the list, it is included in the list,
!    and N, X and INDX are updated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the current list.
!
!    real ( kind = 8 ) X(N), the list.
!
!    integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    real ( kind = 8 ) XVAL, the value to be sought.
!
!  Output:
!
!    integer ( kind = 4 ) N, the updated size of the current list.
!
!    real ( kind = 8 ) X(N), the updated list.
!
!    integer ( kind = 4 ) INDX(N), the updated sort index of the list.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) indx(*)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) more
  real ( kind = 8 ) x(*)
  real ( kind = 8 ) xval

  if ( n <= 0 ) then
    n = 1
    x(1) = xval
    indx(1) = 1
    return
  end if
!
!  Does XVAL already occur in X?
!
  call r8vec_index_search ( n, x, indx, xval, less, equal, more )

  if ( equal == 0 ) then
    x(n+1) = xval
    indx(n+1:more+1:-1) = indx(n:more:-1)
    indx(more) = n + 1
    n = n + 1
  end if

  return
end
subroutine r8vec_index_order ( n, x, indx )

!*****************************************************************************80
!
!! R8VEC_INDEX_ORDER sorts an R8VEC using an index vector.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The index vector itself is not modified.  Therefore, the pair
!    (X,INDX) no longer represents an index sorted vector.  If this
!    relationship is to be preserved, then simply set INDX(1:N)=(1:N).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the current list.
!
!    real ( kind = 8 ) X(N), the list.
!
!    integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the sorted list.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) indx(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  y(1:n) = x(indx(1:n))
  x(1:n) = y(1:n)

  return
end
subroutine r8vec_index_search ( n, x, indx, xval, less, equal, more )

!*****************************************************************************80
!
!! R8VEC_INDEX_SEARCH searches for a value in an indexed sorted R8VEC.
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
!    11 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the current list.
!
!    real ( kind = 8 ) X(N), the list.
!
!    integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    real ( kind = 8 ) XVAL, the value to be sought.
!
!  Output:
!
!    integer ( kind = 4 ) LESS, EQUAL, MORE, the indexes in INDX of the
!    entries of X that are just less than, equal to, and just greater
!    than XVAL.  If XVAL does not occur in X, then EQUAL is zero.
!    If XVAL is the minimum entry of X, then LESS is 0.  If XVAL
!    is the greatest entry of X, then MORE is N+1.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) equal
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) less
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) mid
  integer ( kind = 4 ) more
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xmid
  real ( kind = 8 ) xval

  if ( n <= 0 ) then
    less = 0
    equal = 0
    more = 0
    return
  end if

  lo = 1
  hi = n
  xlo = x(indx(lo))
  xhi = x(indx(hi))

  if ( xval < xlo ) then
    less = 0
    equal = 0
    more = 1
    return
  else if ( xval == xlo ) then
    less = 0
    equal = 1
    more = 2
    return
  end if

  if ( xhi < xval ) then
    less = n
    equal = 0
    more = n + 1
    return
  else if ( xval == xhi ) then
    less = n - 1
    equal = n
    more = n + 1
    return
  end if

  do

    if ( lo + 1 == hi ) then
      less = lo
      equal = 0
      more = hi
      return
    end if

    mid = ( lo + hi ) / 2
    xmid = x(indx(mid))

    if ( xval == xmid ) then
      equal = mid
      less = equal - 1
      more = equal + 1
      return
    else if ( xval < xmid ) then
      hi = mid
    else if ( xmid < xval ) then
      lo = mid
    end if

  end do

  return
end
subroutine r8vec_index_sort_unique ( n, x, indx, n2 )

!*****************************************************************************80
!
!! R8VEC_INDEX_SORT_UNIQUE creates a sorted unique index for an R8VEC.
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
!    11 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the current list.
!
!    real ( kind = 8 ) X(N), the list.
!
!  Output:
!
!    real ( kind = 8 ) X(N), now contains only unique elements.
!
!    integer ( kind = 4 ) INDX(N), the sort index of the list.
!
!    integer ( kind = 4 ) N2, the number of unique elements in X.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) n2
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  n2 = 0

  do i = 1, n
    call r8vec_index_insert_unique ( n2, y, indx, x(i) )
  end do

  x(1:n2) = y(1:n2)

  x(n2+1:n) = 0.0D+00
  indx(n2+1:n) = 0

  return
end
subroutine r8vec_index_sorted_range ( n, r, indx, r_lo, r_hi, i_lo, i_hi )

!*****************************************************************************80
!
!! R8VEC_INDEX_SORTED_RANGE: search index sorted vector for elements in a range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 September 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items in the vector.
!
!    real ( kind = 8 ) R(N), the index sorted vector.
!
!    integer ( kind = 4 ) INDX(N), the vector used to sort R.
!    The vector R(INDX(*)) is sorted.
!
!    real ( kind = 8 ) R_LO, R_HI, the limits of the range.
!
!  Output:
!
!    integer ( kind = 4 ) I_LO, I_HI, the range of indices
!    so that I_LO <= I <= I_HI => R_LO <= R(INDX(I)) <= R_HI.  If no
!    values in R lie in the range, then I_HI < I_LO will be returned.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i_hi
  integer ( kind = 4 ) i_lo
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) r_hi
  real ( kind = 8 ) r_lo
!
!  Cases we can handle immediately.
!
  if ( r(indx(n)) < r_lo ) then
    i_lo = n + 1
    i_hi = n
    return
  end if

  if ( r_hi < r(indx(1)) ) then
    i_lo = 1
    i_hi = 0
    return
  end if
!
!  Are there are least two intervals?
!
  if ( n == 1 ) then
    if ( r_lo <= r(indx(1)) .and. r(indx(1)) <= r_hi ) then
      i_lo = 1
      i_hi = 1
    else
      i_lo = 0
      i_hi = -1
    end if
    return
  end if
!
!  Bracket R_LO.
!
  if ( r_lo <= r(indx(1)) ) then

    i_lo = 1

  else
!
!  R_LO is in one of the intervals spanned by R(INDX(J1)) to R(INDX(J2)).
!  Examine the intermediate interval [R(INDX(I1)), R(INDX(I1+1))].
!  Does R_LO lie here, or below or above?
!
    j1 = 1
    j2 = n
    i1 = ( j1 + j2 - 1 ) / 2
    i2 = i1 + 1

    do

      if ( r_lo < r(indx(i1)) ) then
        j2 = i1
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else if ( r(indx(i2)) < r_lo ) then
        j1 = i2
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else
        i_lo = i1
        exit
      end if

    end do

  end if
!
!  Bracket R_HI.
!
  if ( r(indx(n)) <= r_hi ) then

    i_hi = n

  else

    j1 = i_lo
    j2 = n
    i1 = ( j1 + j2 - 1 ) / 2
    i2 = i1 + 1

    do

      if ( r_hi < r(indx(i1)) ) then
        j2 = i1
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else if ( r(indx(i2)) < r_hi ) then
        j1 = i2
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else
        i_hi = i2
        exit
      end if

    end do

  end if
!
!  We expect to have computed the largest I_LO and smallest I_HI such that
!    R(INDX(I_LO)) <= R_LO <= R_HI <= R(INDX(I_HI))
!  but what we want is actually
!    R_LO <= R(INDX(I_LO)) <= R(INDX(I_HI)) <= R_HI
!  which we can usually get simply by incrementing I_LO and decrementing I_HI.
!
  if ( r(indx(i_lo)) < r_lo ) then
    i_lo = i_lo + 1
    if ( n < i_lo ) then
      i_hi = i_lo - 1
    end if
  end if

  if ( r_hi < r(indx(i_hi)) ) then
    i_hi = i_hi - 1
    if ( i_hi < 1 ) then
      i_lo = i_hi + 1
    end if
  end if

  return
end
subroutine r8vec_indexed_heap_d ( n, a, indx )

!*****************************************************************************80
!
!! R8VEC_INDEXED_HEAP_D creates a descending heap from an indexed R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An indexed R8VEC is an R8VEC of data values, and an R8VEC of N indices,
!    each referencing an entry of the data vector.
!
!    The function adjusts the index vector INDX so that, for 1 <= J <= N/2,
!    we have:
!      A(INDX(2*J))   <= A(INDX(J))
!    and
!      A(INDX(2*J+1)) <= A(INDX(J))
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the index array.
!
!    real ( kind = 8 ) A(*), the data vector.
!
!    integer ( kind = 4 ) INDX(N), the index array.
!    Each entry of INDX must be a valid index for the array A.
!
!  Output:
!
!    integer ( kind = 4 ) INDX(N), the descending heap sorted index array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifree
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) key
  integer ( kind = 4 ) m
!
!  Only nodes N/2 down to 1 can be "parent" nodes.
!
  do i = n / 2, 1, -1
!
!  Copy the value out of the parent node.
!  Position IFREE is now "open".
!
    key = indx(i)
    ifree = i

    do
!
!  Positions 2*IFREE and 2*IFREE + 1 are the descendants of position
!  IFREE.  (One or both may not exist because they exceed N.)
!
      m = 2 * ifree
!
!  Does the first position exist?
!
      if ( n < m ) then
        exit
      end if
!
!  Does the second position exist?
!
      if ( m + 1 <= n ) then
!
!  If both positions exist, take the larger of the two values,
!  and update M if necessary.
!
        if ( a(indx(m)) < a(indx(m+1)) ) then
          m = m + 1
        end if

      end if
!
!  If the large descendant is larger than KEY, move it up,
!  and update IFREE, the location of the free position, and
!  consider the descendants of THIS position.
!
      if ( a(indx(m)) <= a(key) ) then
        exit
      end if

      indx(ifree) = indx(m)
      ifree = m

    end do
!
!  Once there is no more shifting to do, KEY moves into the free spot IFREE.
!
    indx(ifree) = key

  end do

  return
end
subroutine r8vec_indexed_heap_d_extract ( n, a, indx, indx_extract )

!*****************************************************************************80
!
!! R8VEC_INDEXED_HEAP_D_EXTRACT: extract from heap descending indexed R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An indexed R8VEC is an R8VEC of data values, and an R8VEC of N indices,
!    each referencing an entry of the data vector.
!
!    The routine finds the maximum value in the heap, returns that value to the
!    user, deletes that value from the heap, and restores the heap to its
!    proper form.
!
!    Note that the argument N must be a variable, which will be decremented
!    before return, and that INDX will hold one less value on output than it
!    held on input.
!
!    This is one of three functions needed to model a priority queue.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Cormen, Charles Leiserson, Ronald Rivest,
!    Introduction to Algorithms,
!    MIT Press, 2001,
!    ISBN: 0262032937,
!    LC: QA76.C662.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items in the
!    index vector.
!
!    real ( kind = 8 ) A(*), the data vector.
!
!    integer ( kind = 4 ) INDX(N), the index vector.
!
!  Output:
!
!    integer ( kind = 4 ) N, the updated number of items in the
!    index vector.
!
!    integer ( kind = 4 ) INDX(N), the updated index vector.
!
!    integer ( kind = 4 ) INDX_EXTRACT, the index in A of the item of
!    maximum value, which has now been removed from the heap.
!
  implicit none

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) indx(*)
  integer ( kind = 4 ) indx_extract
  integer ( kind = 4 ) n

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_INDEXED_HEAP_D_EXTRACT - Fatal error!'
    write ( *, '(a)' ) '  The heap is empty.'
    stop 1
  end if
!
!  Get the index of the maximum value.
!
  indx_extract = indx(1)

  if ( n == 1 ) then
    n = 0
    return
  end if
!
!  Shift the last index down.
!
  indx(1) = indx(n)
!
!  Restore the heap structure.
!
  n = n - 1
  call r8vec_indexed_heap_d ( n, a, indx )

  return
end
subroutine r8vec_indexed_heap_d_insert ( n, a, indx, indx_insert )

!*****************************************************************************80
!
!! R8VEC_INDEXED_HEAP_D_INSERT: insert value into heap descending indexed R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An indexed R8VEC is an R8VEC of data values, and an R8VEC of N indices,
!    each referencing an entry of the data vector.
!
!    Note that the argument N must be a variable, and will be incremented before
!    return, and that INDX must be able to hold one more entry on output than
!    it held on input.
!
!    This is one of three functions needed to model a priority queue.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Cormen, Charles Leiserson, Ronald Rivest,
!    Introduction to Algorithms,
!    MIT Press, 2001,
!    ISBN: 0262032937,
!    LC: QA76.C662.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items in the
!    index vector.
!
!    real ( kind = 8 ) A(*), the data vector.
!
!    integer ( kind = 4 ) INDX(N), the index vector.
!
!    integer ( kind = 4 ) INDX_INSERT, the index in A of the value
!    to be inserted into the heap.
!
!  Output:
!
!    integer ( kind = 4 ) N, the updated number of items in the
!    index vector.
!
!    integer ( kind = 4 ) INDX(N), the updated index vector.
!
  implicit none

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(*)
  integer ( kind = 4 ) indx_insert
  integer ( kind = 4 ) n
  integer ( kind = 4 ) parent

  n = n + 1
  i = n

  do while ( 1 < i )

    parent = i / 2

    if ( a(indx_insert) <= a(indx(parent)) ) then
      exit
    end if

    indx(i) = indx(parent)
    i = parent

  end do

  indx(i) = indx_insert

  return
end
subroutine r8vec_indexed_heap_d_max ( n, a, indx, indx_max )

!*****************************************************************************80
!
!! R8VEC_INDEXED_HEAP_D_MAX: maximum value in heap descending indexed R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An indexed R8VEC is an R8VEC of data values, and an R8VEC of N indices,
!    each referencing an entry of the data vector.
!
!    This is one of three functions needed to model a priority queue.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Cormen, Charles Leiserson, Ronald Rivest,
!    Introduction to Algorithms,
!    MIT Press, 2001,
!    ISBN: 0262032937,
!    LC: QA76.C662.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items in the index vector.
!
!    real ( kind = 8 ) A(*), the data vector.
!
!    integer ( kind = 4 ) INDX(N), the index vector.
!
!  Output:
!
!    integer ( kind = 4 ) INDX_MAX, the index in A of the maximum value
!    in the heap.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(*)
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indx_max

  call r8_fake_use ( a(1) )

  indx_max = indx(1)

  return
end
subroutine r8vec_indicator0 ( n, a )

!*****************************************************************************80
!
!! R8VEC_INDICATOR0 sets an R8VEC to the indicator vector (0,1,2,...).
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
!    27 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i

  do i = 1, n
    a(i) = real ( i - 1, kind = 8 )
  end do

  return
end
subroutine r8vec_indicator1 ( n, a )

!*****************************************************************************80
!
!! R8VEC_INDICATOR1 sets an R8VEC to the indicator vector (1,2,3,...).
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
!    27 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i

  do i = 1, n
    a(i) = real ( i, kind = 8 )
  end do

  return
end
subroutine r8vec_insert ( n, a, pos, value )

!*****************************************************************************80
!
!! R8VEC_INSERT inserts a value into an R8VEC.
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
!    17 February 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of the array on input.
!
!    real ( kind = 8 ) A(N+1), the array.  On A is
!    assumed to contain only N entries.
!
!    integer ( kind = 4 ) POS, the position to be assigned the new entry.
!    1 <= POS <= N+1.
!
!    real ( kind = 8 ) VALUE, the value to be inserted.
!
!  Output:
!
!    real ( kind = 8 ) A(N+1), the updated array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n+1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) pos
  real ( kind = 8 ) value

  if ( pos < 1 .or. n + 1 < pos ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_INSERT - Fatal error!'
    write ( *, '(a,i8)' ) '  Illegal insertion position = ', pos
    stop 1

  else

    do i = n + 1, pos + 1, -1
      a(i) = a(i-1)
    end do

    a(pos) = value

  end if

  return
end
function r8vec_is_ascending ( n, x )

!*****************************************************************************80
!
!! R8VEC_IS_ASCENDING determines if an R8VEC is (weakly) ascending.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    For example, if:
!
!      X = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.5, 9.8 )
!
!    then
!
!      R8VEC_IS_ASCENDING = TRUE
!
!    The sequence is not required to be strictly ascending.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the array.
!
!    real ( kind = 8 ) X(N), the array to be examined.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_ASCENDING, is TRUE if the
!    entries ascend.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  logical ( kind = 4 ) r8vec_is_ascending
  real ( kind = 8 ) x(n)
  logical ( kind = 4 ) value

  value = .true.
  do i = 1, n - 1
    if ( x(i+1) < x(i) ) then
      value = .false.
      exit
    end if
  end do

  r8vec_is_ascending = value

  return
end
function r8vec_is_ascending_strictly ( n, x )

!*****************************************************************************80
!
!! R8VEC_IS_ASCENDING_STRICTLY determines if an R8VEC is strictly ascending.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Notice the effect of entry number 6 in the following results:
!
!      X = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.4, 9.8 )
!      Y = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.5, 9.8 )
!      Z = ( -8.1, 1.3, 2.2, 3.4, 7.5, 7.6, 9.8 )
!
!      R8VEC_IS_ASCENDING_STRICTLY ( X ) = FALSE
!      R8VEC_IS_ASCENDING_STRICTLY ( Y ) = FALSE
!      R8VEC_IS_ASCENDING_STRICTLY ( Z ) = TRUE
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
!  Input:
!
!    integer ( kind = 4 ) N, the size of the array.
!
!    real ( kind = 8 ) X(N), the array to be examined.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_ASCENDING_STRICTLY, is TRUE if the
!    entries strictly ascend.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  logical ( kind = 4 ) r8vec_is_ascending_strictly
  real ( kind = 8 ) x(n)
  logical ( kind = 4 ) value

  value = .true.
  do i = 1, n - 1
    if ( x(i+1) <= x(i) ) then
      value = .false.
      exit
    end if
  end do

  r8vec_is_ascending_strictly = value

  return
end
function r8vec_is_binary ( n, x )

!*****************************************************************************80
!
!! R8VEC_IS_BINARY is true if an R8VEC only contains 0 and 1 entries.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of the vectors.
!
!    real ( kind = 8 ) X(N), the vector to be checked.
!
!  Output:
!
!    logical R8VEC_IS_BINARY, is true if all entries are 0 or 1.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  logical r8vec_is_binary
  logical value
  real ( kind = 8 ) x(n)

  value = .true.

  do i = 1, n

    if ( x(i) /= 0.0D+00 .and. x(i) /= 1.0D+00 ) then
      value = .false.
      exit
    end if

  end do

  r8vec_is_binary = value

  return
end
function r8vec_is_distinct ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_DISTINCT is true if the entries in an R8VEC are distinct.
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
!    31 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A(N), the vector to be checked.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_DISTINCT is TRUE if the entries 
!    are distinct.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  logical ( kind = 4 ) r8vec_is_distinct
  logical ( kind = 4 ) value

  value = .true.

  do i = 2, n
    do j = 1, i - 1
      if ( a(i) == a(j) ) then
        value = .false.
        exit
      end if
    end do
  end do

  r8vec_is_distinct = value

  return
end
function r8vec_is_in_01 ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_IN_01 is TRUE if the entries of an R8VEC are in the range [0,1].
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
!    06 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_IN_01, is TRUE if every entry is
!    between 0 and 1.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical ( kind = 4 ) r8vec_is_in_01
  logical ( kind = 4 ) value

  if ( any ( a(1:n) < 0.0D+00 .or. 1.0D+00 < a(1:n) ) ) then
    value = .false.
  else
    value = .true.
  end if

  r8vec_is_in_01 = value;

  return
end
function r8vec_is_in_ab ( n, x, a, b )

!*****************************************************************************80
!
!! R8VEC_IS_IN_AB is TRUE if the entries of an R8VEC are in the range [A,B].
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
!    15 April 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in X.
!
!    real ( kind = 8 ) X(N), the vector.
!
!    real ( kind = 8 ) A, B, the limits of the range.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_IN_AB, is TRUE if every entry is
!    between A and B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical ( kind = 4 ) r8vec_is_in_ab
  real ( kind = 8 ) x(n)

  if ( any ( x(1:n) < a .or. b < x(1:n) ) ) then
    r8vec_is_in_ab = .false.
  else
    r8vec_is_in_ab = .true.
  end if

  return
end
function r8vec_is_insignificant ( n, r, s )

!*****************************************************************************80
!
!! R8VEC_IS_INSIGNIFICANT determines if an R8VEC is insignificant.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of the vectors.
!
!    real ( kind = 8 ) R(N), the vector to be compared against.
!
!    real ( kind = 8 ) S(N), the vector to be compared.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_INSIGNIFICANT, is TRUE if S is 
!    insignificant compared to R.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) r(n)
  logical ( kind = 4 ) r8vec_is_insignificant
  real ( kind = 8 ) s(n)
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  logical ( kind = 4 ) value

  value = .true.

  do i = 1, n

    t = r(i) + s(i)
    tol = epsilon ( r(i) ) * abs ( r(i) )

    if ( tol < abs ( r(i) - t ) ) then 
      value = .false.
      exit
    end if

  end do
  
  r8vec_is_insignificant = value

  return
end
function r8vec_is_integer ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_INTEGER is TRUE if the entries of an R8VEC are integers.
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
!    04 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_INTEGER, is TRUE if every entry 
!    is an integer.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical ( kind = 4 ) r8vec_is_integer

  r8vec_is_integer = all ( a(1:n) == aint ( a(1:n) ) )

  return
end
function r8vec_is_negative ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_NEGATIVE: every element of an R8VEC is strictly negative.
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
!    24 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A(N).
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_NEGATIVE, is TRUE every entry 
!    is strictly negative.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical ( kind = 4 ) r8vec_is_negative

  r8vec_is_negative = ( all ( a(1:n) < 0.0D+00 ) )

  return
end
function r8vec_is_negative_any ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_NEGATIVE_ANY: ( any A < 0 ) for R8VEC's.
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
!    08 October 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_NEGATIVE_ANY is TRUE if any entry 
!    is negative.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical ( kind = 4 ) r8vec_is_negative_any

  r8vec_is_negative_any = any ( a(1:n) < 0.0D+00 )

  return
end
function r8vec_is_nonnegative ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_NONNEGATIVE is TRUE if all the entries of an R8VEC are nonnegative.
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
!    29 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_NONNEGATIVE, the value of 
!    the condition.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical ( kind = 4 ) r8vec_is_nonnegative

  r8vec_is_nonnegative = all ( 0.0D+00 <= a(1:n) )

  return
end
function r8vec_is_nonpositive ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_NONPOSITIVE: ( all ( A <= 0 ) ) for R8VEC's.
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
!    08 October 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries.
!
!    double ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_NONPOSITIVE is TRUE if all entries
!    are less than or equal to zero.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical ( kind = 4 ) r8vec_is_nonpositive

  r8vec_is_nonpositive = all ( a(1:n) <= 0.0D+00 )

  return
end
function r8vec_is_nonzero_any ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_NONZERO_ANY: ( any A nonzero ) for R8VEC's.
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
!    25 December 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_NONZERO_ANY is TRUE if any entry 
!    is nonzero.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical ( kind = 4 ) r8vec_is_nonzero_any

  r8vec_is_nonzero_any = any ( a(1:n) /= 0.0D+00 )

  return
end
function r8vec_is_one ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_ONE is TRUE if all the entries of an R8VEC are one.
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
!    29 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_ONE, the value of the condition.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical ( kind = 4 ) r8vec_is_one

  r8vec_is_one = all ( a(1:n) == 1.0D+00 )

  return
end
function r8vec_is_positive ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_POSITIVE: every element of an R8VEC is strictly positive.
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
!    24 June 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A(N).
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_POSITIVE, is TRUE every entry 
!    is strictly positive.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical ( kind = 4 ) r8vec_is_positive

  r8vec_is_positive = ( all ( 0.0D+00 < a(1:n) ) )

  return
end
function r8vec_is_zero ( n, a )

!*****************************************************************************80
!
!! R8VEC_IS_ZERO is TRUE if all the entries of an R8VEC are zero.
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
!    29 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_IS_ZERO, the value of the condition.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical ( kind = 4 ) r8vec_is_zero

  r8vec_is_zero = all ( a(1:n) == 0.0D+00 )

  return
end
subroutine r8vec_legendre ( n, x_first, x_last, x )

!*****************************************************************************80
!
!! R8VEC_LEGENDRE creates a vector of Legendre-spaced values.
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
!    17 June 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) X_FIRST, X_LAST, the first and last entries.
!
!  Output:
!
!    real ( kind = 8 ) X(N), a vector of Legendre-spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_first
  real ( kind = 8 ) x_last

  call legendre_zeros ( n, x )

  x(1:n) = ( ( 1.0D+00 - x(1:n) ) * x_first  &
           + ( 1.0D+00 + x(1:n) ) * x_last ) &
           /   2.0D+00

  return
end
subroutine r8vec_linspace ( n, a, b, x )

!*****************************************************************************80
!
!! r8vec_linspace creates a vector of linearly spaced values.
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
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A, B, the first and last entries.
!
!  Output:
!
!    real ( kind = 8 ) X(N), a vector of linearly spaced data.
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
subroutine r8vec_linspace2 ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_LINSPACE2 creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    5 points evenly spaced between 0 and 12 will yield 2, 4, 6, 8, 10.
!
!    In other words, the interval is divided into N+1 even subintervals,
!    and the endpoints of internal intervals are used as the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A, B, the first and last entries.
!
!  Output:
!
!    real ( kind = 8 ) X(N), a vector of linearly spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, n
    x(i) = ( real ( n  - i + 1, kind = 8 ) * a &
           + real (      i,     kind = 8 ) * b ) &
           / real ( n      + 1, kind = 8 )
  end do

  return
end
function r8vec_lt ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_LT == ( A1 < A2 ) for R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The comparison is lexicographic.
!
!    A1 < A2  <=>                              A1(1) < A2(1) or
!                 ( A1(1)     == A2(1)     and A1(2) < A2(2) ) or
!                 ...
!                 ( A1(1:N-1) == A2(1:N-1) and A1(N) < A2(N)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of the vectors.
!
!    real ( kind = 8 ) A1(N), A2(N), the vectors to be compared.
!
!  Output:
!
!    logical ( kind = 4 ) R8VEC_LT, is TRUE if and only if A1 < A2.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  logical ( kind = 4 ) r8vec_lt
  integer ( kind = 4 ) i

  r8vec_lt = .false.

  do i = 1, n

    if ( a1(i) < a2(i) ) then
      r8vec_lt = .true.
      exit
    else if ( a2(i) < a1(i) ) then
      r8vec_lt = .false.
      exit
    end if

  end do

  return
end
subroutine r8vec_mask_print ( n, a, mask_num, mask, title )

!*****************************************************************************80
!
!! R8VEC_MASK_PRINT prints a masked R8VEC.
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
!    24 September 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of components of the vector.
!
!    real ( kind = 8 ) A(N), the vector to be printed.
!
!    integer ( kind = 4 ) MASK_NUM, the number of masked elements.
!
!    integer ( kind = 4 ) MASK(MASK_NUM), the indices of the vector
!    to be printed.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) mask_num
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) mask(mask_num)
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Masked vector printout:'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, mask_num
    write ( *, '(2x,i8,a,1x,i8,2x,g14.6)' ) i, ':', mask(i), a(mask(i))
  end do

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
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_MAX, the value of the largest entry.
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
subroutine r8vec_max_abs_index ( n, a, max_index )

!*****************************************************************************80
!
!! R8VEC_MAX_ABS_INDEX: index of the maximum absolute value in an R8VEC.
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
!    02 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    integer ( kind = 4 ) MAX_INDEX, the index of the largest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_index

  if ( n <= 0 ) then

    max_index = -1

  else

    max_index = 1

    do i = 2, n
      if ( abs ( a(max_index) ) < abs ( a(i) ) ) then
        max_index = i
      end if
    end do

  end if

  return
end
subroutine r8vec_max_index ( n, a, max_index )

!*****************************************************************************80
!
!! R8VEC_MAX_INDEX returns the index of the maximum value in an R8VEC.
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
!    02 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    integer ( kind = 4 ) MAX_INDEX, the index of the largest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_index

  if ( n <= 0 ) then

    max_index = -1

  else

    max_index = 1

    do i = 2, n
      if ( a(max_index) < a(i) ) then
        max_index = i
      end if
    end do

  end if

  return
end
subroutine r8vec_mean ( n, a, mean )

!*****************************************************************************80
!
!! R8VEC_MEAN returns the mean of an R8VEC.
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
!    02 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A(N), the vector whose mean is desired.
!
!  Output:
!
!    real ( kind = 8 ) MEAN, the mean of the vector entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) mean

  mean = sum ( a(1:n) ) / real ( n, kind = 8 )

  return
end
subroutine r8vec_mean_geometric ( n, a, mean )

!*****************************************************************************80
!
!! R8VEC_MEAN_GEOMETRIC returns the geometric mean of an R8VEC.
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
!    27 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A(N), the vector whose mean is desired.
!    All entries should be nonnegative.
!
!  Output:
!
!    real ( kind = 8 ) MEAN, the geometric mean of the vector entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) mean

  mean = exp ( sum ( log ( a(1:n) ) ) / real ( n, kind = 8 ) )

  return
end
subroutine r8vec_mean_running ( n, v, a )

!*****************************************************************************80
!
!! R8VEC_MEAN_RUNNING computes the running means of an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 February 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items.
!
!    real ( kind = 8 ) V(N), the data.
!
!  Output:
!
!    real ( kind = 8 ) A(N+1), the running average.  A(I) is the 
!    average value of the first I-1 values in V.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n+1)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)
!
!  Sum.
!
  a(1) = 0.0D+00
  do i = 2, n + 1
    a(i) = a(i-1) + v(i-1)
  end do
!
!  Average.
!
  do i = 2, n + 1
    a(i) = a(i) / real ( i - 1, kind = 8 )
  end do

  return
end
subroutine r8vec_mean_update ( nm1, mean_nm1, xn, n, mean_n )

!*****************************************************************************80
!
!! R8VEC_MEAN_UPDATE updates the mean of an R8VEC with one new value.
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
!    29 November 2017
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) NM1, the number of entries in the old vector.
!
!    real ( kind = 8 ) MEAN_NM1, the mean of the old vector.
!
!    real ( kind = 8 ) XN, the new N-th entry of the vector.
!
!  Output:
!
!    integer ( kind = 4 ) N, the number of entries in the new vector.
!
!    real ( kind = 8 ) MEAN_N, the mean of the new vector.
!
  implicit none

  real ( kind = 8 ) mean_n
  real ( kind = 8 ) mean_nm1
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  real ( kind = 8 ) xn

  if ( nm1 <= 0 ) then
    n = 1
    mean_n = xn
  else
    n = nm1 + 1
    mean_n = mean_nm1 + ( xn - mean_nm1 ) / real ( n, kind = 8 )
  end if

  return
end
subroutine r8vec_median ( n, a, median )

!*****************************************************************************80
!
!! R8VEC_MEDIAN returns the median of an unsorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Hoare's algorithm is used.  The values of the vector are
!    rearranged by this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2000
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!    real ( kind = 8 ) A(N), the array to search.
!
!  Output:
!
!    real ( kind = 8 ) A(N), has been rearranged somewhat.
!
!    real ( kind = 8 ) MEDIAN, the value of the median of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) k
  real ( kind = 8 ) median

  k = ( n + 1 ) / 2

  call r8vec_frac ( n, a, k, median )

  return
end
subroutine r8vec_mesh_2d ( nx, ny, xvec, yvec, xmat, ymat )

!*****************************************************************************80
!
!! R8VEC_MESH_2D creates a 2D mesh from X and Y vectors.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    NX = 2
!    XVEC = ( 1, 2, 3 )
!    NY = 3
!    YVEC = ( 4, 5 )
!
!    XMAT = (
!      1, 2, 3
!      1, 2, 3 )
!
!    YMAT = (
!      4, 4, 4
!      5, 5, 5 ) 
!    
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 July 2013
!
!  Input:
!
!    integer ( kind = 4 ) NX, NY, the number of X and Y values.
!
!    real ( kind = 8 ) XVEC(NX), YVEC(NY), the X and Y coordinate
!    values.
!
!  Output:
!
!    real ( kind = 8 ) XMAT(NX,NY), YMAT(NX,NY), the coordinate
!    values of points on an NX by NY mesh.
!
  implicit none

  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny

  integer ( kind = 4 ) j
  real ( kind = 8 ) xmat(nx,ny)
  real ( kind = 8 ) xvec(nx)
  real ( kind = 8 ) ymat(nx,ny)
  real ( kind = 8 ) yvec(ny)

  do j = 1, ny
    xmat(1:nx,j) = xvec(1:nx)
  end do

  do j = 1, ny
    ymat(1:nx,j) = yvec(j)
  end do

 return
end
subroutine r8vec_midspace ( n, a, b, x )

!*****************************************************************************80
!
!! R8VEC_MIDSPACE creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This function divides the interval [a,b] into n subintervals, and then
!    returns the midpoints of those subintervals.
!
!  Example:
!
!    N = 5, A = 10, B = 20
!    X = [ 11, 13, 15, 17, 19 ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 June 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A, B, the endpoints of the interval.
!
!  Output:
!
!    real ( kind = 8 ) X(N), a vector of linearly spaced data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, n
    x(i) = ( real ( 2 * n - 2 * i + 1, kind = 8 ) * a &
           + real (         2 * i - 1, kind = 8 ) * b ) &
           / real ( 2 * n,             kind = 8 )
  end do

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
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_MIN, the value of the smallest entry.
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
subroutine r8vec_min_index ( n, a, min_index )

!*****************************************************************************80
!
!! R8VEC_MIN_INDEX returns the index of the minimum value in an R8VEC.
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
!    02 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    integer ( kind = 4 ) MIN_INDEX, the index of the smallest entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) min_index

  if ( n <= 0 ) then

    min_index = -1

  else

    min_index = 1

    do i = 2, n
      if ( a(i) < a(min_index) ) then
        min_index = i
      end if
    end do

  end if

  return
end
function r8vec_min_pos ( n, a )

!*****************************************************************************80
!
!! R8VEC_MIN_POS returns the minimum positive value of an R8VEC.
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
!    08 November 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_MIN_POS, the smallest positive entry.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec_min_pos
  real ( kind = 8 ) value

  value = huge ( value )

  do i = 1, n
    if ( 0.0D+00 < a(i) ) then
      value = min ( value, a(i) )
    end if
  end do

  r8vec_min_pos = value

  return
end
subroutine r8vec_mirror_next ( n, a, done )

!*****************************************************************************80
!
!! R8VEC_MIRROR_NEXT steps through all sign variations of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In normal use, the user would set every element of A to be positive.
!    The routine will take the input value of A, and output a copy in
!    which the signs of one or more entries have been changed.  Repeatedly
!    calling the routine with the output from the previous call will generate
!    every distinct "variation" of A; that is, all possible sign variations.
!
!    When the output variable DONE is TRUE (or equal to 1), then the
!    output value of A_NEW is the last in the series.
!
!    Note that A may have some zero values.  The routine will essentially
!    ignore such entries; more exactly, it will not stupidly assume that -0
!    is a proper "variation" of 0!
!
!    Also, it is possible to call this routine with the signs of A set
!    in any way you like.  The routine will operate properly, but it
!    will nonethess terminate when it reaches the value of A in which
!    every nonzero entry has negative sign.
!
!    More efficient algorithms using the Gray code seem to require internal
!    memory in the routine, which is not one of MATLAB's strong points,
!    or the passing back and forth of a "memory array", or the use of
!    global variables, or unnatural demands on the user.  This form of
!    the routine is about as clean as I can make it.
!
!  Example:
!
!      Input         Output
!    ---------    --------------
!    A            A_NEW     DONE
!    ---------    --------  ----
!     1  2  3     -1  2  3  false
!    -1  2  3      1 -2  3  false
!     1 -2  3     -1 -2  3  false
!    -1 -2  3      1  2 -3  false
!     1  2 -3     -1  2 -3  false
!    -1  2 -3      1 -2 -3  false
!     1 -2 -3     -1 -2 -3  false
!    -1 -2 -3      1  2  3  true
!
!     1  0  3     -1  0  3  false
!    -1  0  3      1  0 -3  false
!     1  0 -3     -1  0 -3  false
!    -1  0 -3      1  0  3  true
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A(N), a vector of real numbers.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the signs of some entries may have changed.
!
!    logical ( kind = 4 ) DONE, is TRUE if the input vector A was the 
!    last element in the series (every entry was nonpositive); the output 
!    vector is reset so that all entries are nonnegative, but presumably the 
!    ride is over!
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical ( kind = 4 ) done
  integer ( kind = 4 ) i
  integer ( kind = 4 ) positive
!
!  Seek the first strictly positive entry of A.
!
  positive = 0
  do i = 1, n
    if ( 0.0D+00 < a(i) ) then
      positive = i
      exit
    end if
  end do
!
!  If there is no strictly positive entry of A, there is no successor.
!
  if ( positive == 0 ) then
    a(1:n) = - a(1:n)
    done = .true.
    return
  end if
!
!  Otherwise, negate A up to the positive entry.
!
  a(1:positive) = - a(1:positive)
  done = .false.

  return
end
subroutine r8vec_mirror_ab_next ( m, a, b, x, done )

!*****************************************************************************80
!
!! R8VEC_MIRROR_AB_NEXT steps through "mirrored" versions of vector X.
!
!  Discussion:
!
!    X is an M component vector contained in a rectangle described by A and B,
!    that is, for each index I, we have
!      A(I) <= X(I) <= B(I).
!
!    "Mirrored" versions of the vector X have one or more components
!    reflected about the A or B limit.  
!
!    As long as each component of X is strictly between the limits A and B,
!    this means there will be 3^M possible versions of the vector.
!
!    If one component of X is equal to one limit or the other, this 
!    suppresses mirroring across that limit.  If one component of
!    X, A and B are equal, then no mirroring is done at all in that component.
! 
!  Example:
!
!      A = 0, 0, 0
!      X = 1, 1, 1
!      B = 2, 2, 2
!      results in the following sequence of 3x3x3 values:
!
!      0  0  0
!      0  0  1
!      0  0  2
!      0  1  0
!      0  1  1
!      .......
!      2  1  1
!      2  1  2
!      2  2  0
!      2  2  1
!      2  2  2
!
!    A = 0 1 0
!    X = 1 1 1
!    B = 2 2 2
!    results in the following sequence of 3x2x3 values:
!
!      0 1 0
!      0 1 1
!      0 1 2
!      0 2 0
!      0 2 1
!      0 2 2
!      1 1 0
!      1 1 1
!      1 1 2
!      1 2 0
!      1 2 1
!      1 2 2
!      2 1 0
!      2 1 1
!      2 1 2
!      2 2 0
!      2 2 1
!      2 2 2
!
!    A = 0 1 0
!    X = 1 1 1
!    B = 2 1 2
!    results in the following sequence of 3x1x3 values:
!
!      0 1 0
!      0 1 1
!      0 1 2
!      1 1 0
!      1 1 1
!      1 1 2
!      2 1 0
!      2 1 1
!      2 1 2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the number of entries in the vector.
!
!    real ( kind = 8 ) A(M), B(M), the lower and upper limits.
!
!    real ( kind = 8 ) X(M), a vector being manipulated.
!
!    logical DONE.  On first call, DONE should be TRUE, and
!    A(I) <= X(I) <= B(I) for each index I.  On if DONE is TRUE,
!    then the returned value is the last entry in the sequence.
!
!  Output:
!
!    real ( kind = 8 ) X(M), the modified vector.
!
!    logical DONE.  If DONE is TRUE,
!    then the returned value is the last entry in the sequence.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) a(m)
  real ( kind = 8 ) b(m)
  logical done
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(m)
!
!  First call:
!
  if ( done ) then
!
!  Ensure all A(I) <= X(I) <= B(I).
!
    do i = 1, m

      if ( x(i) < a(i) ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'R8VEC_MIRROR_AB_NEXT - Fatal error!'
        write ( *, '(a)' ) '  Not every A(I) <= X(I).'
        stop 1
      end if

      if ( b(i) < x(i) ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'R8VEC_MIRROR_AB_NEXT - Fatal error!'
        write ( *, '(a)' ) '  Not every X(I) <= B(I).'
        stop 1
      end if

    end do
!
!  Set first element of sequence.
!
    x(1:m) = 2.0D+00 * a(1:m) - x(1:m)
!
!  Unless A = B, our sequence is not done.
!
    done = .true.
    do i = 1, m
      if ( a(i) /= b(i) ) then
        done = .false.
        exit
      end if
    end do
!
!  Subsequent calls.
!
  else
!
!  Initialize index to last.
!  loop
!    if index < 1, set DONE = true and return.
!    if the index-th value is below B, increment it and return;
!    otherwise reset index-th value to A.
!    decrement the index.
!
    i = m

    do while ( 1 <= i )

      if ( x(i) < a(i) ) then
        x(i) = 2.0D+00 * a(i) - x(i)
        return
      else if ( x(i) < b(i) ) then
        x(i) = 2.0D+00 * b(i) - x(i)
        return
      else
        x(i) = x(i) - 2.0D+00 * ( b(i) - a(i) )
      end if

      i = i - 1

    end do

    done = .true.

  end if

  return
end
subroutine r8vec_mm_to_01 ( n, a )

!*****************************************************************************80
!
!! R8VEC_MM_TO_01 shifts and rescales an R8VEC to lie within [0,1].
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    On A contains the original data.  On A has been shifted
!    and scaled so that all entries lie between 0 and 1.
!
!  Formula:
!
!    A(I) := ( A(I) - AMIN ) / ( AMAX - AMIN )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of data values.
!
!    real ( kind = 8 ) A(N), the data to be rescaled.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the rescaled data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) amin

  amax = maxval ( a(1:n) )
  amin = minval ( a(1:n) )

  if ( amin == amax ) then
    a(1:n) = 0.5D+00
  else
    a(1:n) = ( a(1:n) - amin ) / ( amax - amin )
  end if

  return
end
subroutine r8vec_mm_to_cd ( n, a, bmin, bmax, b )

!*****************************************************************************80
!
!! R8VEC_MM_TO_CD shifts and rescales an R8VEC from one interval to another.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The mininum entry of A is mapped to BMIN, the maximum entry
!    to BMAX, and values in between are mapped linearly.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of data values.
!
!    real ( kind = 8 ) A(N), the data to be remapped.
!
!    real ( kind = 8 ) BMIN, BMAX, the values to which min(A) and max(A)
!    are to be assigned.
!
!  Output:
!
!    real ( kind = 8 ) B(N), the remapped data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) amin
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) bmax
  real ( kind = 8 ) bmin

  if ( bmax == bmin ) then
    b(1:n) = bmin
    return
  end if

  amin = minval ( a(1:n) )
  amax = maxval ( a(1:n) )

  if ( amax == amin ) then
    b(1:n) = 0.5D+00 * ( bmax + bmin )
    return
  end if

  b(1:n) = ( ( amax - a(1:n)        ) * bmin   &
         + (          a(1:n) - amin ) * bmax ) &
           / ( amax          - amin )

  return
end
subroutine r8vec_nint ( n, a )

!*****************************************************************************80
!
!! R8VEC_NINT rounds entries of an R8VEC.
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
!    10 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A(N), the vector to be NINT'ed.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the NINT'ed vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)

  a(1:n) = nint ( real ( a(1:n), kind = 8 ) )

  return
end
function r8vec_norm ( n, a )

!*****************************************************************************80
!
!! r8vec_norm returns the L2 norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L2 norm is defined as:
!
!      R8VEC_NORM = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector whose L2 norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_NORM, the L2 norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm

  r8vec_norm = sqrt ( sum ( a(1:n) ** 2 ) )

  return
end
function r8vec_norm_affine ( n, v0, v1 )

!*****************************************************************************80
!
!! R8VEC_NORM_AFFINE returns the affine norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The affine vector L2 norm is defined as:
!
!      R8VEC_NORM_AFFINE(V0,V1)
!        = sqrt ( sum ( 1 <= I <= N ) ( V1(I) - V0(I) )^2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the vectors.
!
!    real ( kind = 8 ) V0(N), the base vector.
!
!    real ( kind = 8 ) V1(N), the vector whose affine norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_NORM_AFFINE, the L2 norm of V1-V0.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) r8vec_norm_affine
  real ( kind = 8 ) v0(n)
  real ( kind = 8 ) v1(n)

  r8vec_norm_affine = sqrt ( sum ( ( v0(1:n) - v1(1:n) ) ** 2 ) )

  return
end
function r8vec_norm_l0 ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM_L0 returns the l0 "norm" of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The l0 "norm" simply counts the number of nonzero entries in the vector.
!    It is not a true norm, but has some similarities to one.  It is useful
!    in the study of compressive sensing.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 June 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_NORM_L0, the value of the norm.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) r8vec_norm_l0
  real ( kind = 8 ) value

  value = 0.0D+00
  do i = 1, n
    if ( a(i) /= 0.0D+00 ) then
      value = value + 1.0D+00
    end if
  end do

  r8vec_norm_l0 = value

  return
end
function r8vec_norm_l1 ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM_L1 returns the L1 norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L1 norm is defined as:
!
!      R8VEC_NORM_L1 = sum ( 1 <= I <= N ) abs ( A(I) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2002
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector whose L1 norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_NORM_L1, the L1 norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm_l1

  r8vec_norm_l1 = sum ( abs ( a(1:n) ) )

  return
end
function r8vec_norm_l2 ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM_L2 returns the L2 norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L2 norm is defined as:
!
!      R8VEC_NORM_L2 = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2002
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector whose L2 norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_NORM_L2, the L2 norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm_l2

  r8vec_norm_l2 = sqrt ( sum ( a(1:n) ** 2 ) )

  return
end
function r8vec_norm_li ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM_LI returns the Loo norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector Loo norm is defined as:
!
!      R8VEC_NORM_LI = max ( 1 <= I <= N ) abs ( A(I) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2002
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector whose Loo norm is desired.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_NORM_LI, the Loo norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm_li

  r8vec_norm_li = maxval ( abs ( a(1:n) ) )

  return
end
function r8vec_norm_lp ( n, a, p )

!*****************************************************************************80
!
!! R8VEC_NORM_LP returns the LP norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector LP norm is defined as:
!
!      R8VEC_NORM_LP = ( sum ( 1 <= I <= N ) ( abs ( A(I) ) )^P )^(1/P).
!
!    Usually, the LP norms with
!      1 <= P <= oo
!    are of interest.  This routine allows
!      0 < P <= Huge ( P ).
!    If P = Huge ( P ), then the Loo norm is returned, which is
!    simply the maximum of the absolute values of the vector components.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2002
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector whose LP norm is desired.
!
!    real ( kind = 8 ) P, the index of the norm.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_NORM_LP, the LP norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) p
  real ( kind = 8 ) r8vec_norm_lp

  if ( p <= 0.0D+00 ) then
    r8vec_norm_lp = -1.0D+00
  else if ( p == huge ( p ) ) then
    r8vec_norm_lp = maxval ( abs ( a(1:n) ) )
  else if ( p == 1.0D+00 ) then
    r8vec_norm_lp = sum ( abs ( a(1:n) ) )
  else if ( p == 2.0D+00 ) then
    r8vec_norm_lp = sqrt ( sum ( a(1:n) ** 2 ) )
  else
    r8vec_norm_lp = ( sum ( ( abs ( a(1:n) ) ) ** p ) ) ** ( 1.0D+00 / p )
  end if

  return
end
function r8vec_norm_rms ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM_RMS returns the RMS norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector RMS norm is defined as:
!
!      R8VEC_NORM_RMS = sqrt ( sum ( 1 <= I <= N ) A(I)^2 / N ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_NORM_RMS, the RMS norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm_rms

  r8vec_norm_rms = sqrt ( sum ( a(1:n) ** 2 ) / real ( n, kind = 8 ) )

  return
end
function r8vec_norm_squared ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORM_SQUARED returns the square of the L2 norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    R8VEC_NORM_SQUARED = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_NORM_SQUARED, the squared L2 norm of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_norm_squared

  r8vec_norm_squared = sum ( a(1:n) ** 2 )

  return
end
subroutine r8vec_normal_01 ( n, seed, x )

!*****************************************************************************80
!
!! R8VEC_NORMAL_01 returns a unit pseudonormal R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    This routine can generate a vector of values on one call.  It
!    has the feature that it should provide the same results
!    in the same order no matter how we break up the task.
!
!    Before calling this routine, the user may call RANDOM_SEED
!    in order to set the seed of the random number generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of values desired.  
!
!    integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!  Output:
!
!    real ( kind = 8 ) X(N), a sample of the standard normal PDF.
!
!    integer ( kind = 4 ) SEED, an updated seed value.
!
!  Local:
!
!    real ( kind = 8 ) R(N+1), is used to store some uniform
!    random values.  Its dimension is N+1, but really it is only needed
!    to be the smallest even number greater than or equal to N.
!
!    integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
!    X that we need to compute.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) m
  real ( kind = 8 ) r(n+1)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)
  integer ( kind = 4 ) x_hi_index
  integer ( kind = 4 ) x_lo_index
!
!  Record the range of X we need to fill in.
!
  x_lo_index = 1
  x_hi_index = n
!
!  Maybe we don't need any more values.
!
  if ( x_hi_index - x_lo_index + 1 == 1 ) then

    r(1) = r8_uniform_01 ( seed )

    if ( r(1) == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORMAL_01 - Fatal error!'
      write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
      stop 1
    end if

    r(2) = r8_uniform_01 ( seed )

    x(x_hi_index) = &
             sqrt ( -2.0D+00 * log ( r(1) ) ) * cos ( 2.0D+00 * r8_pi * r(2) )
!
!  If we require an even number of values, that's easy.
!
  else if ( mod ( x_hi_index - x_lo_index + 1, 2 ) == 0 ) then

    m = ( x_hi_index - x_lo_index + 1 ) / 2

    call r8vec_uniform_01 ( 2*m, seed, r )

    x(x_lo_index:x_hi_index-1:2) = &
      sqrt ( -2.0D+00 * log ( r(1:2*m-1:2) ) ) &
      * cos ( 2.0D+00 * r8_pi * r(2:2*m:2) )

    x(x_lo_index+1:x_hi_index:2) = &
      sqrt ( -2.0D+00 * log ( r(1:2*m-1:2) ) ) &
      * sin ( 2.0D+00 * r8_pi * r(2:2*m:2) )
!
!  If we require an odd number of values, we generate an even number,
!  and handle the last pair specially, storing one in X(N), and
!  saving the other for later.
!
  else

    x_hi_index = x_hi_index - 1

    m = ( x_hi_index - x_lo_index + 1 ) / 2 + 1

    call r8vec_uniform_01 ( 2 * m, seed, r )

    x(x_lo_index:x_hi_index-1:2) = &
      sqrt ( -2.0D+00 * log ( r(1:2*m-3:2) ) ) &
      * cos ( 2.0D+00 * r8_pi * r(2:2*m-2:2) )

    x(x_lo_index+1:x_hi_index:2) = &
      sqrt ( -2.0D+00 * log ( r(1:2*m-3:2) ) ) &
      * sin ( 2.0D+00 * r8_pi * r(2:2*m-2:2) )

    x(n) = sqrt ( -2.0D+00 * log ( r(2*m-1) ) ) &
      * cos ( 2.0D+00 * r8_pi * r(2*m) )

  end if

  return
end
subroutine r8vec_normal_ab ( n, a, b, seed, x )

!*****************************************************************************80
!
!! R8VEC_NORMAL_AB returns a scaled pseudonormal R8VEC.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of values desired.
!
!    real ( kind = 8 ) A, B, the mean and standard deviation.
!
!    integer ( kind = 4 ) SEED, a seed for the random number generator.
!
!  Output:
!
!    real ( kind = 8 ) X(N), a sample of the standard normal PDF.
!
!    integer ( kind = 4 ) SEED, an updated seed value.
!
!  Local:
!
!    real ( kind = 8 ) R(N+1), is used to store some uniform
!    random values.  Its dimension is N+1, but really it is only needed
!    to be the smallest even number greater than or equal to N.
!
!    integer ( kind = 4 ) X_LO_INDEX, X_HI_INDEX, records the range
!    of entries of X that we need to compute. 
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) m
  real ( kind = 8 ) r(n+1)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)
  integer ( kind = 4 ) x_hi_index
  integer ( kind = 4 ) x_lo_index
!
!  Record the range of X we need to fill in.
!
  x_lo_index = 1
  x_hi_index = n
!
!  If we need just one new value, do that here to avoid null arrays.
!
  if ( x_hi_index - x_lo_index + 1 == 1 ) then

    r(1) = r8_uniform_01 ( seed )

    if ( r(1) == 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORMAL_AB - Fatal error!'
      write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
      stop 1
    end if

    r(2) = r8_uniform_01 ( seed )

    x(x_hi_index) = &
      sqrt ( - 2.0D+00 * log ( r(1) ) ) * cos ( 2.0D+00 * r8_pi * r(2) )
!
!  If we require an even number of values, that's easy.
!
  else if ( mod ( x_hi_index - x_lo_index, 2 ) == 1 ) then

    m = ( x_hi_index - x_lo_index + 1 ) / 2

    call r8vec_uniform_01 ( 2*m, seed, r )

    x(x_lo_index:x_hi_index-1:2) = &
      sqrt ( - 2.0D+00 * log ( r(1:2*m-1:2) ) ) &
      * cos ( 2.0D+00 * r8_pi * r(2:2*m:2) )

    x(x_lo_index+1:x_hi_index:2) = &
      sqrt ( - 2.0D+00 * log ( r(1:2*m-1:2) ) ) &
      * sin ( 2.0D+00 * r8_pi * r(2:2*m:2) )
!
!  If we require an odd number of values, we generate an even number,
!  and handle the last pair specially, storing one in X(N), and
!  saving the other for later.
!
  else

    x_hi_index = x_hi_index - 1

    m = ( x_hi_index - x_lo_index + 1 ) / 2 + 1

    call r8vec_uniform_01 ( 2*m, seed, r )

    x(x_lo_index:x_hi_index-1:2) = &
      sqrt ( - 2.0D+00 * log ( r(1:2*m-3:2) ) ) &
      * cos ( 2.0D+00 * r8_pi * r(2:2*m-2:2) )

    x(x_lo_index+1:x_hi_index:2) = &
      sqrt ( - 2.0D+00 * log ( r(1:2*m-3:2) ) ) &
      * sin ( 2.0D+00 * r8_pi * r(2:2*m-2:2) )

    x(n) = sqrt ( - 2.0D+00 * log ( r(2*m-1) ) ) &
      * cos ( 2.0D+00 * r8_pi * r(2*m) )

  end if

  x(1:n) = a + b * x(1:n)

  return
end
subroutine r8vec_normalize ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORMALIZE normalizes an R8VEC in the Euclidean norm.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The euclidean norm is also sometimes called the l2 or
!    least squares norm.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of the vector.
!
!    real ( kind = 8 ) A(N), the vector to be normalized.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the normalized vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) norm

  norm = sqrt ( sum ( a(1:n) ** 2 ) )

  if ( norm /= 0.0D+00 ) then
    a(1:n) = a(1:n) / norm
  end if

  return
end
subroutine r8vec_normalize_l1 ( n, a )

!*****************************************************************************80
!
!! R8VEC_NORMALIZE_L1 normalizes an R8VEC to have unit sum.
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
!    10 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A(N), the vector to be normalized.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the normalized vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_sum

  a_sum = sum ( a(1:n) )

  if ( a_sum == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_NORMALIZE_L1 - Fatal error!'
    write ( *, '(a)' ) '  The vector entries sum to 0.'
    stop 1
  end if

  a(1:n) = a(1:n) / a_sum

  return
end
function r8vec_normsq ( n, v )

!*****************************************************************************80
!
!! R8VEC_NORMSQ returns the square of the L2 norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The square of the vector L2 norm is defined as:
!
!      R8VEC_NORMSQ = sum ( 1 <= I <= N ) V(I)^2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the vector dimension.
!
!    real ( kind = 8 ) V(N), the vector.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_NORMSQ, the squared L2 norm.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) r8vec_normsq
  real ( kind = 8 ) v(n)

  r8vec_normsq = sum ( v(1:n) ** 2 )

  return
end
function r8vec_normsq_affine ( n, v0, v1 )

!*****************************************************************************80
!
!! R8VEC_NORMSQ_AFFINE returns the affine squared norm of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The affine squared vector L2 norm is defined as:
!
!      R8VEC_NORMSQ_AFFINE(V0,V1)
!        = sum ( 1 <= I <= N ) ( V1(I) - V0(I) )^2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the vector dimension.
!
!    real ( kind = 8 ) V0(N), the base vector.
!
!    real ( kind = 8 ) V1(N), the vector.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_NORMSQ_AFFINE, the squared affine L2 norm.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) r8vec_normsq_affine
  real ( kind = 8 ) v0(n)
  real ( kind = 8 ) v1(n)

  r8vec_normsq_affine = sum ( ( v0(1:n) - v1(1:n) ) ** 2 )

  return
end
subroutine r8vec_order_type ( n, a, order )

!*****************************************************************************80
!
!! R8VEC_ORDER_TYPE determines if R8VEC is (non)strictly ascending/descending.
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
!    15 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of the array.
!
!    real ( kind = 8 ) A(N), the array to be checked.
!
!  Output:
!
!    integer ( kind = 4 ) ORDER, order indicator:
!    -1, no discernable order;
!    0, all entries are equal;
!    1, ascending order;
!    2, strictly ascending order;
!    3, descending order;
!    4, strictly descending order.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) order
!
!  Search for the first value not equal to A(1).
!
  i = 1

  do

    i = i + 1

    if ( n < i ) then
      order = 0
      return
    end if

    if ( a(1) < a(i) ) then

      if ( i == 2 ) then
        order = 2
      else
        order = 1
      end if

      exit

    else if ( a(i) < a(1) ) then

      if ( i == 2 ) then
        order = 4
      else
        order = 3
      end if

      exit

    end if

  end do
!
!  Now we have a "direction".  Examine subsequent entries.
!
  do while ( i < n )

    i = i + 1

    if ( order == 1 ) then

      if ( a(i) < a(i-1) ) then
        order = -1
        exit
      end if

    else if ( order == 2 ) then

      if ( a(i) < a(i-1) ) then
        order = -1
        exit
      else if ( a(i) == a(i-1) ) then
        order = 1
      end if

    else if ( order == 3 ) then

      if ( a(i-1) < a(i) ) then
        order = -1
        exit
      end if

    else if ( order == 4 ) then

      if ( a(i-1) < a(i) ) then
        order = -1
        exit
      else if ( a(i) == a(i-1) ) then
        order = 3
      end if

    end if

  end do

  return
end
subroutine r8vec_part_quick_a ( n, a, l, r )

!*****************************************************************************80
!
!! R8VEC_PART_QUICK_A reorders an R8VEC as part of a quick sort.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine reorders the entries of A.  Using A(1) as the key,
!    all entries of A that are less than or equal to the key will
!    precede the key which precedes all entries that are greater than the key.
!
!  Example:
!
!    Input:
!
!      N = 8
!
!      A = ( 6, 7, 3, 1, 6, 8, 2, 9 )
!
!    Output:
!
!      L = 3, R = 6
!
!      A = ( 3, 1, 2, 6, 6, 8, 9, 7 )
!            -------        -------
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of A.
!
!    real ( kind = 8 ) A(N), the array to be checked.
!
!  Output:
!
!    real ( kind = 8 ) A(N), has been reordered as described above.
!
!    integer ( kind = 4 ) L, R, the indices of A that define
!    the three segments.  Let KEY = the input value of A(1).  Then
!    I <= L                 A(I) < KEY;
!         L < I < R         A(I) = KEY;
!                 R <= I    KEY < A(I).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) key
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) r
  real ( kind = 8 ) temp

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_PART_QUICK_A - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    stop 1
  else if ( n == 1 ) then
    l = 0
    r = 2
    return
  end if

  key = a(1)
  m = 1
!
!  The elements of unknown size have indices between L+1 and R-1.
!
  l = 1
  r = n + 1

  do i = 2, n

    if ( key < a(l+1) ) then
      r = r - 1
      temp = a(r)
      a(r) = a(l+1)
      a(l+1) = temp
    else if ( a(l+1) == key ) then
      m = m + 1
      temp = a(m)
      a(m) = a(l+1)
      a(l+1) = temp
      l = l + 1
    else if ( a(l+1) < key ) then
      l = l + 1
    end if

  end do
!
!  Now shift small elements to the left, and KEY elements to center.
!
  do i = 1, l - m
    a(i) = a(i+m)
  end do
!
!  Out of bounds here, occasionally
!
  l = l - m

  a(l+1:l+m) = key

  return
end
subroutine r8vec_permute ( n, p, a )

!*****************************************************************************80
!
!! R8VEC_PERMUTE permutes an R8VEC in place.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This routine permutes an array of real "objects", but the same
!    logic can be used to permute an array of objects of any arithmetic
!    type, or an array of objects of any complexity.  The only temporary
!    storage required is enough to store a single object.  The number
!    of data movements made is N + the number of cycles of order 2 or more,
!    which is never more than N + N/2.
!
!    P(I) = J means that the I-th element of the output array should be
!    the J-th element of the input array.  P must be a legal permutation
!    of the integers from 1 to N, otherwise the algorithm will
!    fail catastrophically.
!
!  Example:
!
!    Input:
!
!      N = 5
!      P = (   2,   4,   5,   1,   3 )
!      A = ( 1.0, 2.0, 3.0, 4.0, 5.0 )
!
!    Output:
!
!      A    = ( 2.0, 4.0, 5.0, 1.0, 3.0 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of objects.
!
!    integer ( kind = 4 ) P(N), the permutation.
!
!    real ( kind = 8 ) A(N), the array to be permuted.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the permuted array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_temp
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) istart
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) perm1_check

  ierror = perm1_check ( n, p )

  if ( ierror .ne. 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'R8VEC_PERMUTE - Fatal error!'
    write ( *, '(a)' ) '  PERM1_CHECK returned error.'
    stop 1
  end if
!
!  Search for the next element of the permutation that has not been used.
!
  do istart = 1, n

    if ( p(istart) < 0 ) then

      cycle

    else if ( p(istart) == istart ) then

      p(istart) = - p(istart)
      cycle

    else

      a_temp = a(istart)
      iget = istart
!
!  Copy the new value into the vacated entry.
!
      do

        iput = iget
        iget = p(iget)

        p(iput) = - p(iput)

        if ( iget < 1 .or. n < iget ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8VEC_PERMUTE - Fatal error!'
          write ( *, '(a)' ) '  A permutation index is out of range.'
          write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
          stop 1
        end if

        if ( iget == istart ) then
          a(iput) = a_temp
          exit
        end if

        a(iput) = a(iget)

      end do

    end if

  end do
!
!  Restore the signs of the entries.
!
  p(1:n) = - p(1:n)

  return
end
subroutine r8vec_permute_cyclic ( n, k, a )

!*****************************************************************************80
!
!! R8VEC_PERMUTE_CYCLIC performs a cyclic permutation of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    For 0 <= K < N, this function cyclically permutes the input vector
!    to have the form
!
!     ( A(K+1), A(K+2), ..., A(N), A(1), ..., A(K) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of objects.
!
!    integer ( kind = 4 ) K, the increment used.
!
!    real ( kind = 8 ) A(N), the array to be permuted.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the permuted array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_wrap
  integer ( kind = 4 ) ipk
  integer ( kind = 4 ) k

  do i = 1, n
    ipk = i4_wrap ( i + k, 1, n )
    b(i) = a(ipk)
  end do

  a(1:n) = b(1:n)

  return
end
subroutine r8vec_permute_uniform ( n, a, seed )

!*****************************************************************************80
!
!! R8VEC_PERMUTE_UNIFORM randomly permutes an R8VEC.
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
!    01 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of objects.
!
!    real ( kind = 8 ) A(N), the array to be permuted.
!
!    integer ( kind = 4 ) SEED, a seed for the random number generator.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the permuted array.
!
!    integer ( kind = 4 ) SEED, an updated seed value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) p(n)
  integer ( kind = 4 ) seed

  call perm1_uniform ( n, seed, p )

  call r8vec_permute ( n, p, a )

  return
end
subroutine r8vec_polarize ( n, a, p, a_normal, a_parallel )

!*****************************************************************************80
!
!! R8VEC_POLARIZE decomposes an R8VEC into normal and parallel components.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The (nonzero) vector P defines a direction.
!
!    The vector A can be written as the sum
!
!      A = A_normal + A_parallel
!
!    where A_parallel is a linear multiple of P, and A_normal
!    is perpendicular to P.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the vector to be polarized.
!
!    real ( kind = 8 ) P(N), the polarizing direction.
!
!  Output:
!
!    real ( kind = 8 ) A_NORMAL(N), A_PARALLEL(N), the normal
!    and parallel components of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_dot_p
  real ( kind = 8 ) a_normal(n)
  real ( kind = 8 ) a_parallel(n)
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) p_norm

  p_norm = sqrt ( sum ( p(1:n) ** 2 ) )

  if ( p_norm == 0.0D+00 ) then
    a_normal(1:n) = a(1:n)
    a_parallel(1:n) = 0.0D+00
    return
  end if

  a_dot_p = dot_product ( a(1:n), p(1:n) ) / p_norm

  a_parallel(1:n) = a_dot_p * p(1:n) / p_norm

  a_normal(1:n) = a(1:n) - a_parallel(1:n)

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
!  Input:
!
!    integer ( kind = 4 ) N, the number of components of the vector.
!
!    real ( kind = 8 ) A(N), the vector to be printed.
!
!    character ( len = * ) TITLE, a title.
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
subroutine r8vec_print_16 ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_PRINT_16 prints an R8VEC to 16 decimal places.
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
!    29 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of components of the vector.
!
!    real ( kind = 8 ) A(N), the vector to be printed.
!
!    character ( len = * ) TITLE, a title.
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
    write ( *, '(2x,i8,a,1x,g24.16)' ) i, ':', a(i)
  end do

  return
end
subroutine r8vec_print_part ( n, a, i_lo, i_hi, title )

!*****************************************************************************80
!
!! R8VEC_PRINT_PART prints "part" of an R8VEC.
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
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of the vector.
!
!    real ( kind = 8 ) A(N), the vector to be printed.
!
!    integer ( kind = 4 ) I_LO, I_HI, the first and last indices
!    to print.  The routine expects 1 <= I_LO <= I_HI <= N.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_hi
  integer ( kind = 4 ) i_lo
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )

  write ( *, '(a)' ) ' '
  do i = max ( i_lo, 1 ), min ( i_hi, n )
    write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
  end do

  return
end
subroutine r8vec_print_some ( n, a, max_print, title )

!*****************************************************************************80
!
!! R8VEC_PRINT_SOME prints "some" of an R8VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of the vector.
!
!    real ( kind = 8 ) A(N), the vector to be printed.
!
!    integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * ) title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(2x,i8,a,1x,g14.6)' ) i, ':', a(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,a,1x,g14.6)' ) i, ':', a(i)
    end do
    write ( *, '(a)' ) '  ........  ..............'
    i = n
    write ( *, '(2x,i8,a,1x,g14.6)' ) i, ':', a(i)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,a,1x,g14.6)' ) i, ':', a(i)
    end do
    i = max_print
    write ( *, '(2x,i8,a,1x,g14.6,2x,a)' ) i, ':', a(i), '...more entries...'

  end if

  return
end
subroutine r8vec_print2 ( n, a )

!*****************************************************************************80
!
!! R8VEC_PRINT2 prints out an R8VEC.
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
!    26 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of A.
!
!    real ( kind = 8 ) A(N), the vector to be printed.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) amin
  integer ( kind = 4 ) i
  character ( len = 11 ) iform
  logical ( kind = 4 ) integ
  integer ( kind = 4 ) lmax
  real ( kind = 8 ) r8_log_10
!
!  Check if all entries are integral.
!
  integ = .true.

  do i = 1, n

    if ( a(i) /= real ( int ( a(i) ), kind = 8 ) ) then
      integ = .false.
      exit
    end if

  end do
!
!  Find the range of the array.
!
  amax = maxval ( abs ( a(1:n) ) )
  amin = minval ( abs ( a(1:n) ) )
!
!  Use the information about the maximum size of an entry to
!  compute an intelligent format for use with integer entries.
!
!  Later, we might also do this for real vectors.
!
  lmax = int ( r8_log_10 ( amax ) )

  if ( integ ) then
    write ( iform, '( ''(2x,i'', i2, '')'' )' ) lmax + 3
  else
    iform = ' '
  end if

  do i = 1, n

    if ( integ ) then
      write ( *, iform ) int ( a(i) )
    else
      write ( *, '(2x,g14.6)' ) a(i)
    end if

  end do

  return
end
function r8vec_product ( n, a )

!*****************************************************************************80
!
!! R8VEC_PRODUCT returns the product of the entries of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In FORTRAN90, this facility is offered by the built in
!    PRODUCT function:
!
!      R8VEC_PRODUCT ( N, A ) = PRODUCT ( A(1:N) )
!
!    In MATLAB, this facility is offered by the built in
!    PROD function:
!
!      R8VEC_PRODUCT ( N, A ) = PROD ( A(1:N) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_PRODUCT, the product of the entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_product

  r8vec_product = product ( a(1:n) )

  return
end
subroutine r8vec_range ( n, x, xmin, xmax, y, ymin, ymax )

!*****************************************************************************80
!
!! R8VEC_RANGE finds the range of Y's within a restricted X range.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine is given a set of pairs of points (X,Y), and a range
!    XMIN to XMAX of valid X values.  Over this range, it seeks
!    YMIN and YMAX, the minimum and maximum values of Y for
!    valid X's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) X(N), the X array.
!
!    real ( kind = 8 ) XMIN, XMAX, the range of X values to check.
!
!    real ( kind = 8 ) Y(N), the Y array.
!
!  Output:
!
!    real ( kind = 8 ) YMIN, YMAX, the range of Y values whose
!    X value is within the X range.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) ymax
  real ( kind = 8 ) ymin

  ymin =   huge ( ymin )
  ymax = - huge ( ymax )

  do i = 1, n

    if ( xmin <= x(i) .and. x(i) <= xmax ) then

      ymin = min ( ymin, y(i) )
      ymax = max ( ymax, y(i) )

    end if

  end do

  return
end
subroutine r8vec_range_2 ( n, a, amin, amax )

!*****************************************************************************80
!
!! R8VEC_RANGE_2 updates a range to include a new array.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Given a range AMIN to AMAX, and an array A, the routine will
!    decrease AMIN if necessary, or increase AMAX if necessary, so that
!    every entry of A is between AMIN and AMAX.
!
!    However, AMIN will not be increased, nor AMAX decreased.
!
!    This routine may be used to compute the maximum and minimum of a
!    collection of arrays one at a time.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!    real ( kind = 8 ) AMIN, AMAX, the
!    current legal range of values for A.  
!
!  Output:
!
!    real ( kind = 8 ) AMIN, AMAX, either unchanged, or else "widened" 
!    so that all entries of A are within the range.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) amax
  real ( kind = 8 ) amin

  amax = max ( amax, maxval ( a(1:n) ) )
  amin = min ( amin, minval ( a(1:n) ) )

  return
end
subroutine r8vec_reverse ( n, a )

!*****************************************************************************80
!
!! R8VEC_REVERSE reverses the elements of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In FORTRAN90, calling R8VEC_REVERSE is equivalent to
!
!      A(1:N) = A(N:1:-1)
!
!  Example:
!
!    Input:
!
!      N = 5,
!      A = ( 11.0, 12.0, 13.0, 14.0, 15.0 ).
!
!    Output:
!
!      A = ( 15.0, 14.0, 13.0, 12.0, 11.0 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array to be reversed.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the reversed array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)

  a(1:n) = a(n:1:-1)

  return
end
subroutine r8vec_rotate ( n, a, m )

!*****************************************************************************80
!
!! R8VEC_ROTATE "rotates" the entries of an R8VEC in place.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    This routine rotates an array of real "objects", but the same
!    logic can be used to permute an array of objects of any arithmetic
!    type, or an array of objects of any complexity.  The only temporary
!    storage required is enough to store a single object.  The number
!    of data movements made is N + the number of cycles of order 2 or more,
!    which is never more than N + N/2.
!
!  Example:
!
!    Input:
!
!      N = 5, M = 2
!      A    = ( 1.0, 2.0, 3.0, 4.0, 5.0 )
!
!    Output:
!
!      A    = ( 4.0, 5.0, 1.0, 2.0, 3.0 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of objects.
!
!    integer ( kind = 4 ) M, the number of positions to the right that
!    each element should be moved.  Elements that shift pass position
!    N "wrap around" to the beginning of the array.
!
!    real ( kind = 8 ) A(N), the array to be rotated.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the rotated array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i4_modp
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  integer ( kind = 4 ) istart
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mcopy
  integer ( kind = 4 ) nset
  real ( kind = 8 ) temp
!
!  Force M to be positive, between 0 and N-1.
!
  mcopy = i4_modp ( m, n )

  if ( mcopy == 0 ) then
    return
  end if

  istart = 0
  nset = 0

  do

    istart = istart + 1

    if ( n < istart ) then
      exit
    end if

    temp = a(istart)
    iget = istart
!
!  Copy the new value into the vacated entry.
!
    do

      iput = iget

      iget = iget - mcopy
      if ( iget < 1 ) then
        iget = iget + n
      end if

      if ( iget == istart ) then
        exit
      end if

      a(iput) = a(iget)
      nset = nset + 1

    end do

    a(iput) = temp
    nset = nset + 1

    if ( n <= nset ) then
      exit
    end if

  end do

  return
end
function r8vec_rsquared ( n, y_data, y_model )

!*****************************************************************************80
!
!! R8VEC_RSQUARED computes the R^2 goodness of fit measurement.
!
!  Discussion:
!
!    We suppose a set of N data values Y_DATA are known, and that a
!    formula or model has generated a corresponding set of Y_MODEL values.
!
!    R^2 measures the extent to which the variation in Y_DATA is captured
!    by the model data Y_MODEL.  If the model is linear, then a value
!    of R^2=1 is optimal, and R^2=0 is the worst possible.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 January 2019
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of values.
!
!    real ( kind = 8 ) Y_DATA(N), Y_MODEL(N), the data and model values.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_RSQUARED, the R^2 value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) bot
  real ( kind = 8 ) r8vec_rsquared
  real ( kind = 8 ) top
  real ( kind = 8 ) y_average
  real ( kind = 8 ) y_data(n)
  real ( kind = 8 ) y_model(n)

  y_average = sum ( y_data(1:n) ) / real ( n, kind = 8 )

  top = sum ( y_data(1:n) - y_model(1:n) ) ** 2
  bot = sum ( y_data(1:n) - y_average ) ** 2

  r8vec_rsquared = 1.0D+00 - top / bot

  return
end
function r8vec_rsquared_adjusted ( n, y_data, y_model, degree )

!*****************************************************************************80
!
!! R8VEC_RSQUARED_ADJUSTED computes the adjusted R^2 goodness of fit measurement.
!
!  Discussion:
!
!    We suppose a set of N data values Y_DATA are known, and that a
!    formula or model has generated a corresponding set of Y_MODEL values.
!
!    R^2 measures the extent to which the variation in Y_DATA is captured
!    by the model data Y_MODEL.  
!
!    The adjusted value of R^2 accounts for the use of a polynomial model
!    of degree higher than 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2019
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of values.
!
!    real ( kind = 8 ) Y_DATA(N), Y_MODEL(N), the data and model values.
!
!    integer ( kind = 4 ) DEGREE, the polynomial degree of the model.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_RSQUARED_ADJUSTED, the adjusted R^2 value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) bot
  integer ( kind = 4 ) degree
  real ( kind = 8 ) r8vec_rsquared_adjusted
  real ( kind = 8 ) top
  real ( kind = 8 ) y_average
  real ( kind = 8 ) y_data(n)
  real ( kind = 8 ) y_model(n)

  y_average = sum ( y_data(1:n) ) / real ( n, kind = 8 )

  top = sum ( y_data(1:n) - y_model(1:n) ) ** 2
  bot = sum ( y_data(1:n) - y_average ) ** 2

  r8vec_rsquared_adjusted = 1.0D+00 - ( top / bot ) &
    * real ( n - 1, kind = 8 ) / real ( n - degree - 1, kind = 8 )

  return
end
function r8vec_scalar_triple_product ( v1, v2, v3 )

!*****************************************************************************80
!
!! R8VEC_SCALAR_TRIPLE_PRODUCT computes the scalar triple product.
!
!  Discussion:
!
!    STRIPLE = V1 dot ( V2 x V3 ).
!
!    STRIPLE is the volume of the parallelogram whose sides are
!    formed by V1, V2 and V3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) V1(3), V2(3), V3(3), the three vectors.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_SCALAR_TRIPLE_PRODUCT, the scalar
!    triple product.
!
  implicit none

  real ( kind = 8 ) r8vec_scalar_triple_product
  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)

  r8vec_scalar_triple_product = &
      v1(1) * ( v2(2) * v3(3) - v2(3) * v3(2) ) &
    + v1(2) * ( v2(3) * v3(1) - v2(1) * v3(3) ) &
    + v1(3) * ( v2(1) * v3(2) - v2(2) * v3(1) )

  return
end
subroutine r8vec_scale ( s, n, x )

!*****************************************************************************80
!
!! R8VEC_SCALE multiplies an R8VEC by a scale factor.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) S, the scale factor.
!
!    integer ( kind = 4 ) N, the length of the vector.
!
!    real ( kind = 8 ) X(N), the vector to be scaled.
!
!  Output:
!
!     real ( kind = 8 ) X(N), the scaled vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) s
  real ( kind = 8 ) x(n)

  x(1:n) = s * x(1:n)

  return
end
subroutine r8vec_scale_01 ( n, x, xs )

!*****************************************************************************80
!
!! R8VEC_SCALE_01 scales an R8VEC to [0,1].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) X(N), the vector to be scaled.
!
!  Output:
!
!    real ( kind = 8 ) XS(N), the scaled vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ) xs(n)

  xs(1:n) = x(1:n)
  xmin = minval ( xs )
  xmax = maxval ( xs )
  if ( 0.0D+00 < xmax - xmin ) then
    xs = ( xs - xmin ) / ( xmax - xmin )
  else
    xs = 0.5D+00
  end if

  return
end
subroutine r8vec_scale_ab ( n, x, a, b, xs )

!*****************************************************************************80
!
!! R8VEC_SCALE_AB scales an R8VEC to [A,B].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) X(N), the vector to be scaled.
!
!    real ( kind = 8 ) A, B, the limits.
!
!  Output:
!
!    real ( kind = 8 ) XS(N), the scaled vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xmax
  real ( kind = 8 ) xmin
  real ( kind = 8 ) xs(n)

  xs(1:n) = x(1:n)
  xmin = minval ( xs )
  xmax = maxval ( xs )
  if ( 0.0D+00 < xmax - xmin ) then
    xs = a + ( b - a ) * ( xs - xmin ) / ( xmax - xmin )
  else
    xs = ( a + b ) / 2.0D+00
  end if

  return
end
subroutine r8vec_search_binary_a ( n, a, aval, indx )

!*****************************************************************************80
!
!! R8VEC_SEARCH_BINARY_A searches an ascending sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Binary search is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Kreher, Douglas Simpson,
!    Algorithm 1.9,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 26.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements in the array.
!
!    real ( kind = 8 ) A(N), the array to be searched.  The array must
!    be sorted in ascending order.
!
!    real ( kind = 8 ) AVAL, the value to be searched for.
!
!  Output:
!
!    integer ( kind = 4 ) INDX, the result of the search.
!    -1, AVAL does not occur in the array.
!    I, A(I) = AVAL.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) aval
  integer ( kind = 4 ) high
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) low
  integer ( kind = 4 ) mid

  indx = -1

  low = 1
  high = n

  do while ( low <= high )

    mid = ( low + high ) / 2

    if ( a(mid) == aval ) then
      indx = mid
      exit
    else if ( a(mid) < aval ) then
      low = mid + 1
    else if ( aval < a(mid) ) then
      high = mid - 1
    end if

  end do

  return
end
subroutine r8vec_shift ( shift, n, x )

!*****************************************************************************80
!
!! R8VEC_SHIFT performs a shift on an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) SHIFT, the amount by which each entry is to
!    be shifted.
!
!    integer ( kind = 4 ) N, the length of the vector.
!
!    real ( kind = 8 ) X(N), the vector to be shifted.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the shifted vector.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) shift
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  y(1:n) = x(1:n)

  x(1:n) = 0.0D+00

  ilo = max ( 1, 1 + shift )
  ihi = min ( n, n + shift )

  x(ilo:ihi) = y(ilo-shift:ihi-shift)

  return
end
subroutine r8vec_shift_circular ( shift, n, x )

!*****************************************************************************80
!
!! R8VEC_SHIFT_CIRCULAR performs a circular shift on an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) SHIFT, the amount by which each entry is to
!    be shifted.
!
!    integer ( kind = 4 ) N, the length of the vector.
!
!    real ( kind = 8 ) X(N), the vector to be shifted.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the shifted vector.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_wrap
  integer ( kind = 4 ) j
  integer ( kind = 4 ) shift
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  y(1:n) = x(1:n)

  do i = 1, n
    j = i4_wrap ( i - shift, 1, n )
    x(i) = y(j)
  end do

  return
end
subroutine r8vec_sign3_running ( n, v, s )

!*****************************************************************************80
!
!! R8VEC_SIGN3_RUNNING computes the running threeway sign of an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 February 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items.
!
!    real ( kind = 8 ) V(N), the data.
!
!  Output:
!
!    real ( kind = 8 ) S(N+1), the running threeway sign.  S(I) is:
!    -1.0, if the sum of the first I-1 values in V is negative
!     0.0, if zero
!    +1.0, if positive.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) s(n+1)
  real ( kind = 8 ) v(n)
!
!  Sum.
!
  s(1) = 0.0D+00
  do i = 2, n + 1
    s(i) = s(i-1) + v(i-1)
  end do

  do i = 1, n + 1
    if ( s(i) < 0.0D+00 ) then
      s(i) = -1.0D+00
    else if ( s(i) == 0.0D+00 ) then
      s(i) = 0.0D+00
    else if ( 0.0D+00 < s(i) ) then
      s(i) = +1.0D+00
    end if
  end do

  return
end
subroutine r8vec_smooth ( n, x, s, z )

!*****************************************************************************80
!
!! r8vec_smooth smooths an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Except for the beginning and ending entries, the vector values
!    are replaced by averages of 2*S+1 neighbors.
!
!  Example:
!
!    S = 2
!
!    Z(1)   =                     X(1)
!    Z(2)   = (          X(1)   + X(2)   + X(3) ) / 3
!    Z(3)   = ( X(1)   + X(2)   + X(3)   + X(4)   + X(5) ) / 5
!    Z(4)   = ( X(2)   + X(3)   + X(4)   + X(5)   + X(6) ) / 5
!    ...
!    Z(N-2) = ( X(N-4) + X(N-3) + X(N-2) + X(N-1) + X(N) ) / 5
!    Z(N-1) =          ( X(N-2) + X(N-1) + X(N) ) / 3
!    Z(N) =                       X(N)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2019
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the dimension of X.
!
!    real ( kind = 8 ) X(N), the vector to be smoothed.
!
!  Output:
!
!    real ( kind = 8 ) Z(N), the smoothed vector.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  integer ( kind = 4 ) s
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) z(n)
!
!  Work with temporary vector Y, so that user can identify X and Z.
!
  do j = 1, s
    y(j) = sum ( x(1:2*j-1) ) / real ( 2 * j - 1, kind = 8 )
  end do

  do j = s + 1, n - s
    y(j) = sum ( x(j-s:j+s) ) / real ( 2 * s + 1, kind = 8 )
  end do

  do j = s, 1, -1
    y(n+1-j) = sum ( x(n+1-(2*j-1):n) ) / real ( 2 * j - 1, kind = 8 )
  end do

  z(1:n) = y(1:n)

  return
end
subroutine r8vec_softmax ( n, x, s )

!*****************************************************************************80
!
!! R8VEC_SOFTMAX evaluates the SOFTMAX function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 August 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the vector.
!
!    real ( kind = 8 ) X(N), the vector.
!
!  Output:
!
!    real ( kind = 8 ) VALUE(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) bottom
  integer ( kind = 4 ) i
  real ( kind = 8 ) s(n)
  real ( kind = 8 ) x(n)

  call r8vec_max_index ( n, x, i )

  bottom = sum ( exp ( x(1:n) - x(i) ) )

  s(1:n) = exp ( x(1:n) - x(i) ) / bottom

  return
end
subroutine r8vec_sort_bubble_a ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_BUBBLE_A ascending sorts an R8VEC using bubble sort.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Bubble sort is simple to program, but inefficient.  It should not
!    be used for large arrays.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), an unsorted array.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the sorted array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) t

  do i = 1, n - 1
    do j = i + 1, n
      if ( a(j) < a(i) ) then
        t    = a(i)
        a(i) = a(j)
        a(j) = t
      end if
    end do
  end do

  return
end
subroutine r8vec_sort_bubble_d ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_BUBBLE_D descending sorts an R8VEC using bubble sort.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Bubble sort is simple to program, but inefficient.  It should not
!    be used for large arrays.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 January 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), an unsorted array.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the sorted array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) t

  do i = 1, n - 1
    do j = i + 1, n
      if ( a(i) < a(j) ) then
        t    = a(i)
        a(i) = a(j)
        a(j) = t
      end if
    end do
  end do

  return
end
subroutine r8vec_sort_heap_a ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_A ascending sorts an R8VEC using heap sort.
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
!    07 July 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), an unsorted array.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the sorted array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) n1
  real ( kind = 8 ) temp

  if ( n <= 1 ) then
    return
  end if
!
!  1: Put A into descending heap form.
!
  call r8vec_heap_d ( n, a )
!
!  2: Sort A.
!
!  The largest object in the heap is in A(1).
!  Move it to position A(N).
!
  temp = a(1)
  a(1) = a(n)
  a(n) = temp
!
!  Consider the diminished heap of size N1.
!
  do n1 = n - 1, 2, -1
!
!  Restore the heap structure of A(1) through A(N1).
!
    call r8vec_heap_d ( n1, a )
!
!  Take the largest object from A(1) and move it to A(N1).
!
    temp = a(1)
    a(1) = a(n1)
    a(n1) = temp

  end do

  return
end
subroutine r8vec_sort_heap_d ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_D descending sorts an R8VEC using heap sort.
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
!    07 July 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), an unsorted array.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the sorted array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) n1
  real ( kind = 8 ) t

  if ( n <= 1 ) then
    return
  end if
!
!  1: Put A into ascending heap form.
!
  call r8vec_heap_a ( n, a )
!
!  2: Sort A.
!
!  The smallest object in the heap is in A(1).
!  Move it to position A(N).
!
  t    = a(1)
  a(1) = a(n)
  a(n) = t
!
!  Consider the diminished heap of size N1.
!
  do n1 = n - 1, 2, -1
!
!  Restore the heap structure of A(1) through A(N1).
!
    call r8vec_heap_a ( n1, a )
!
!  Take the smallest object from A(1) and move it to A(N1).
!
    t     = a(1)
    a(1)  = a(n1)
    a(n1) = t

  end do

  return
end
subroutine r8vec_sort_heap_index_a ( n, a, indx )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_INDEX_A does an indexed heap ascending sort of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The sorting is not actually carried out.  Rather an index array is
!    created which defines the sorting.  This array may be used to sort
!    or index the array, or to sort or index related arrays keyed on the
!    original array.
!
!    Once the index array is computed, the sorting can be carried out
!    "implicitly:
!
!      A(INDX(I:N)) is sorted,
!
!    or explicitly, by the call
!
!      call r8vec_permute ( n, indx, a )
!
!    after which A(1:N) is sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), an array to be index-sorted.
!
!  Output:
!
!    integer ( kind = 4 ) INDX(N), the sort index.  The
!    I-th element of the sorted array is A(INDX(I)).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) aval
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indxt
  integer ( kind = 4 ) ir
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l

  if ( n < 1 ) then
    return
  end if

  do i = 1, n
    indx(i) = i
  end do

  if ( n == 1 ) then
    return
  end if

  l = n / 2 + 1
  ir = n

  do

    if ( 1 < l ) then

      l = l - 1
      indxt = indx(l)
      aval = a(indxt)

    else

      indxt = indx(ir)
      aval = a(indxt)
      indx(ir) = indx(1)
      ir = ir - 1

      if ( ir == 1 ) then
        indx(1) = indxt
        exit
      end if

    end if

    i = l
    j = l + l

    do while ( j <= ir )

      if ( j < ir ) then
        if ( a(indx(j)) < a(indx(j+1)) ) then
          j = j + 1
        end if
      end if

      if ( aval < a(indx(j)) ) then
        indx(i) = indx(j)
        i = j
        j = j + j
      else
        j = ir + 1
      end if

    end do

    indx(i) = indxt

  end do

  return
end
subroutine r8vec_sort_heap_index_d ( n, a, indx )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_INDEX_D does an indexed heap descending sort of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The sorting is not actually carried out.  Rather an index array is
!    created which defines the sorting.  This array may be used to sort
!    or index the array, or to sort or index related arrays keyed on the
!    original array.
!
!    Once the index array is computed, the sorting can be carried out
!    "implicitly:
!
!      A(INDX(1:N)) is sorted,
!
!    or explicitly, by the call
!
!      call r8vec_permute ( n, indx, a )
!
!    after which A(1:N) is sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), an array to be index-sorted.
!
!  Output:
!
!    integer ( kind = 4 ) INDX(N), the sort index.  The
!    I-th element of the sorted array is A(INDX(I)).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) aval
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indxt
  integer ( kind = 4 ) ir
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l

  if ( n < 1 ) then
    return
  end if

  do i = 1, n
    indx(i) = i
  end do

  if ( n == 1 ) then
    return
  end if

  l = n / 2 + 1
  ir = n

  do

    if ( 1 < l ) then

      l = l - 1
      indxt = indx(l)
      aval = a(indxt)

    else

      indxt = indx(ir)
      aval = a(indxt)
      indx(ir) = indx(1)
      ir = ir - 1

      if ( ir == 1 ) then
        indx(1) = indxt
        exit
      end if

    end if

    i = l
    j = l + l

    do while ( j <= ir )

      if ( j < ir ) then
        if ( a(indx(j+1)) < a(indx(j)) ) then
          j = j + 1
        end if
      end if

      if ( a(indx(j)) < aval ) then
        indx(i) = indx(j)
        i = j
        j = j + j
      else
        j = ir + 1
      end if

    end do

    indx(i) = indxt

  end do

  return
end
subroutine r8vec_sort_heap_mask_a ( n, a, mask_num, mask, indx )

!*****************************************************************************80
!
!! R8VEC_SORT_HEAP_MASK_A: indexed heap ascending sort of a masked R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    An array A is given.  An array MASK of indices into A is given.
!    The routine produces a vector INDX, which is a permutation of the
!    entries of MASK, so that:
!
!      A(MASK(INDX(I)) <= A(MASK(INDX(J))
!
!    whenever
!
!      I <= J
!
!    In other words, only the elements of A that are indexed by MASK
!    are to be considered, and the only thing that happens is that
!    a rearrangment of the indices in MASK is returned that orders the
!    masked elements.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 September 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), an array to be index-sorted.
!
!    integer ( kind = 4 ) MASK_NUM, the number of mask elements.
!
!    integer ( kind = 4 ) MASK(MASK_NUM), the mask array.  This is
!    simply a list of indices of A.  The entries of MASK should
!    be unique, and each one should be between 1 and N.
!
!  Output:
!
!    integer ( kind = 4 ) INDX(MASK_NUM), the sort index.  There are
!    MASK_NUM elements of A selected by MASK.  If we want to list those
!    elements in order, then the I-th element is A(MASK(INDX(I))).
!
  implicit none

  integer ( kind = 4 ) mask_num
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) aval
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(mask_num)
  integer ( kind = 4 ) indxt
  integer ( kind = 4 ) ir
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) mask(mask_num)

  if ( n < 1 ) then
    return
  end if

  if ( mask_num < 1 ) then
    return
  end if

  if ( mask_num == 1 ) then
    indx(1) = 1
    return
  end if

  call i4vec_indicator1 ( mask_num, indx )

  l = mask_num / 2 + 1
  ir = mask_num

  do

    if ( 1 < l ) then

      l = l - 1
      indxt = indx(l)
      aval = a(mask(indxt))

    else

      indxt = indx(ir)
      aval = a(mask(indxt))
      indx(ir) = indx(1)
      ir = ir - 1

      if ( ir == 1 ) then
        indx(1) = indxt
        exit
      end if

    end if

    i = l
    j = l + l

    do while ( j <= ir )

      if ( j < ir ) then
        if ( a(mask(indx(j))) < a(mask(indx(j+1))) ) then
          j = j + 1
        end if
      end if

      if ( aval < a(mask(indx(j))) ) then
        indx(i) = indx(j)
        i = j
        j = j + j
      else
        j = ir + 1
      end if

    end do

    indx(i) = indxt

  end do

  return
end
subroutine r8vec_sort_insert_a ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_INSERT_A ascending sorts an R8VEC using an insertion sort.
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
!    24 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Kreher, Douglas Simpson,
!    Algorithm 1.1,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 11.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items in the vector.
!    N must be positive.
!
!    real ( kind = 8 ) A(N), an unsorted array.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the sorted array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x

  do i = 2, n

    x = a(i)

    j = i - 1

    do while ( 1 <= j )

      if ( a(j) <= x ) then
        exit
      end if

      a(j+1) = a(j)
      j = j - 1

    end do

    a(j+1) = x

  end do

  return
end
subroutine r8vec_sort_insert_index_a ( n, a, indx )

!*****************************************************************************80
!
!! R8VEC_SORT_INSERT_INDEX_A ascending index sorts an R8VEC using insertion.
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
!    06 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Kreher, Douglas Simpson,
!    Algorithm 1.1,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 11.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items in the vector.
!    N must be positive.
!
!    real ( kind = 8 ) A(N), the array to be sorted.
!
!  Output:
!
!    integer ( kind = 4 ) INDX(N), the sorted indices.  The array
!    is sorted when listed from A(INDX(1)) through A(INDX(N)).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) x

  if ( n < 1 ) then
    return
  end if

  do i = 1, n
    indx(i) = i
  end do

  do i = 2, n

    x = a(i)

    j = i - 1

    do while ( 1 <= j )

      if ( a(indx(j)) <= x ) then
        exit
      end if

      indx(j+1) = indx(j)
      j = j - 1

    end do

    indx(j+1) = i

  end do

  return
end
subroutine r8vec_sort_insert_index_d ( n, a, indx )

!*****************************************************************************80
!
!! R8VEC_SORT_INSERT_INDEX_D descending index sorts an R8VEC using insertion.
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
!    07 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Kreher, Douglas Simpson,
!    Algorithm 1.1,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 11.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items in the vector.
!    N must be positive.
!
!    real ( kind = 8 ) A(N), the array to be sorted.
!
!  Output:
!
!    integer ( kind = 4 ) INDX(N), the sorted indices.  The array
!    is sorted when listed from A(INDX(1)) through A(INDX(N)).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) j
  real ( kind = 8 ) x

  if ( n < 1 ) then
    return
  end if

  do i = 1, n
    indx(i) = i
  end do

  do i = 2, n

    x = a(i)

    j = i - 1

    do while ( 1 <= j )

      if ( x <= a(indx(j)) ) then
        exit
      end if

      indx(j+1) = indx(j)
      j = j - 1

    end do

    indx(j+1) = i

  end do

  return
end
subroutine r8vec_sort_quick_a ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_QUICK_A ascending sorts an R8VEC using quick sort.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Example:
!
!    Input:
!
!      N = 7
!      A = ( 6, 7, 3, 2, 9, 1, 8 )
!
!    Output:
!
!      A = ( 1, 2, 3, 6, 7, 8, 9 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), an unsorted array.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the sorted array.
!
  implicit none

  integer ( kind = 4 ), parameter :: level_max = 30
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) base
  integer ( kind = 4 ) l_segment
  integer ( kind = 4 ) level
  integer ( kind = 4 ) n_segment
  integer ( kind = 4 ) rsave(level_max)
  integer ( kind = 4 ) r_segment

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_SORT_QUICK_A - Fatal error!'
    write ( *, '(a)' ) '  N < 1.'
    stop 1
  end if

  if ( n == 1 ) then
    return
  end if

  level = 1
  rsave(level) = n + 1
  base = 1
  n_segment = n

  do
!
!  Partition the segment.
!
    call r8vec_part_quick_a ( n_segment, a(base), l_segment, r_segment )
!
!  If the left segment has more than one element, we need to partition it.
!
    if ( 1 < l_segment ) then

      if ( level_max < level ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8VEC_SORT_QUICK_A - Fatal error!'
        write ( *, '(a,i8)' ) '  Exceeding recursion maximum of ', level_max
        stop 1
      end if

      level = level + 1
      n_segment = l_segment
      rsave(level) = r_segment + base - 1
!
!  The left segment and the middle segment are sorted.
!  Must the right segment be partitioned?
!
    else if ( r_segment < n_segment ) then

      n_segment = n_segment + 1 - r_segment
      base = base + r_segment - 1
!
!  Otherwise, we back up a level if there is an earlier one.
!
    else

      do

        if ( level <= 1 ) then
          return
        end if

        base = rsave(level)
        n_segment = rsave(level-1) - rsave(level)
        level = level - 1

        if ( 0 < n_segment ) then
          exit
        end if

      end do

    end if

  end do

  return
end
subroutine r8vec_sort_shell_a ( n, a )

!*****************************************************************************80
!
!! R8VEC_SORT_SHELL_A ascending sorts an R8VEC using Shell's sort.
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
!    01 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), an unsorted array.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the sorted array.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) asave
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifree
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) ipow
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) maxpow

  if ( n <= 1 ) then
    return
  end if
!
!  Determine the smallest MAXPOW so that
!    N <= ( 3**MAXPOW - 1 ) / 2
!
  maxpow = 1

  do while ( 3 ** maxpow < 2 * n + 1 )
    maxpow = maxpow + 1
  end do

  if ( 1 < maxpow ) then
    maxpow = maxpow - 1
  end if
!
!  Now sort groups of size ( 3^IPOW - 1 ) / 2.
!
  do ipow = maxpow, 1, -1

    inc = ( 3**ipow - 1 ) / 2
!
!  Sort the values with indices equal to K mod INC.
!
    do k = 1, inc
!
!  Insertion sort of the items with index
!  INC+K, 2*INC+K, 3*INC+K, ...
!
      do i = inc+k, n, inc

        asave = a(i)
        ifree = i
        j = i - inc

        do

          if ( j < 1 ) then
            exit
          end if

          if ( a(j) <= asave ) then
            exit
          end if

          ifree = j
          a(j+inc) = a(j)
          j = j - inc

        end do

        a(ifree) = asave

      end do

    end do

  end do

  return
end
subroutine r8vec_sort2_a ( n, x, y )

!*****************************************************************************80
!
!! R8VEC_SORT2_A ascending sorts an R8VEC and adjusts an associated R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The routine sorts the elements of X, and whenever
!    an element of X is moved, the corresponding element of
!    Y is moved in the same way.  This action means that after
!    the sorting, every element of X is still paired to the
!    same Y value.
!
!    If you have more than one array associated with X, or
!    an integer array, or some other complication, you may want to
!    look at doing an "indexed sort" instead.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, length of input array.
!
!    real ( kind = 8 ) X(N), an unsorted array.
!
!    real ( kind = 8 ) Y(N), an array related to X.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the sorted array.
!
!    real ( kind = 8 ) Y(N), the values in Y have been shifted to maintain
!    the correspondence with X.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  if ( n <= 1 ) then
    return
  end if

  i = 0
  indx = 0
  isgn = 0
  j = 0

  do

    call sort_heap_external ( n, indx, i, j, isgn )

    if ( 0 < indx ) then

      t    = x(i)
      x(i) = x(j)
      x(j) = t

      t    = y(i)
      y(i) = y(j)
      y(j) = t

    else if ( indx < 0 ) then

      if ( x(i) <= x(j) ) then
        isgn = -1
      else
        isgn = + 1
      end if

    else if ( indx == 0 ) then

      exit

    end if

  end do

  return
end
subroutine r8vec_sorted_merge_a ( na, a, nb, b, nc, c )

!*****************************************************************************80
!
!! R8VEC_SORTED_MERGE_A merges two ascending sorted R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The elements of A and B should be sorted in ascending order.
!
!    The elements in the output array C will also be in ascending order,
!    and unique.
!
!    The output vector C may share storage with A or B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) NA, the dimension of A.
!
!    real ( kind = 8 ) A(NA), the first sorted array.
!
!    integer ( kind = 4 ) NB, the dimension of B.
!
!    real ( kind = 8 ) B(NB), the second sorted array.
!
!  Output:
!
!    integer ( kind = 4 ) NC, the number of elements in the output
!    array.  Note that C should usually be dimensioned at least NA+NB in the
!    calling routine.
!
!    real ( kind = 8 ) C(NC), the merged unique sorted array.
!
  implicit none

  integer ( kind = 4 ) na
  integer ( kind = 4 ) nb

  real ( kind = 8 ) a(na)
  real ( kind = 8 ) b(nb)
  real ( kind = 8 ) c(na+nb)
  real ( kind = 8 ) d(na+nb)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) ja
  integer ( kind = 4 ) jb
  integer ( kind = 4 ) na2
  integer ( kind = 4 ) nb2
  integer ( kind = 4 ) nc
  integer ( kind = 4 ) order

  na2 = na
  nb2 = nb

  ja = 0
  jb = 0
  nc = 0

  call r8vec_order_type ( na2, a, order )

  if ( order < 0 .or. 2 < order ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_SORTED_MERGE_A - Fatal error!'
    write ( *, '(a)' ) '  The input array A is not ascending sorted!'
    stop 1
  end if

  call r8vec_order_type ( nb2, b, order )

  if ( order < 0 .or. 2 < order ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_SORTED_MERGE_A - Fatal error!'
    write ( *, '(a)' ) '  The input array B is not ascending sorted!'
    stop 1
  end if

  do
!
!  If we've used up all the entries of A, stick the rest of B on the end.
!
    if ( na2 <= ja ) then

      do j = 1, nb2 - jb
        jb = jb + 1
        if ( nc == 0 ) then
          nc = nc + 1
          d(nc) = b(jb)
        else if ( d(nc) < b(jb) ) then
          nc = nc + 1
          d(nc) = b(jb)
        end if
      end do

      c(1:nc) = d(1:nc)

      exit
!
!  If we've used up all the entries of B, stick the rest of A on the end.
!
    else if ( nb2 <= jb ) then

      do j = 1, na2 - ja
        ja = ja + 1
        if ( nc == 0 ) then
          nc = nc + 1
          d(nc) = a(ja)
        else if ( d(nc) < a(ja) ) then
          nc = nc + 1
          d(nc) = a(ja)
        end if
      end do

      c(1:nc) = d(1:nc)

      exit
!
!  Otherwise, if the next entry of A is smaller, that's our candidate.
!
    else if ( a(ja+1) <= b(jb+1) ) then

      ja = ja + 1
      if ( nc == 0 ) then
        nc = nc + 1
        d(nc) = a(ja)
      else if ( d(nc) < a(ja) ) then
        nc = nc + 1
        d(nc) = a(ja)
      end if
!
!  ...or if the next entry of B is the smaller, consider that.
!
    else

      jb = jb + 1
      if ( nc == 0 ) then
        nc = nc + 1
        d(nc) = b(jb)
      else if ( d(nc) < b(jb) ) then
        nc = nc + 1
        d(nc) = b(jb)
      end if
    end if

  end do

  return
end
function r8vec_sorted_nearest ( n, a, value )

!*****************************************************************************80
!
!! R8VEC_SORTED_NEAREST returns the nearest element in a sorted R8VEC.
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
!    29 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!    real ( kind = 8 ) A(N), a sorted vector.
!
!    real ( kind = 8 ) VALUE, the value whose nearest vector
!    entry is sought.
!
!  Output:
!
!    integer ( kind = 4 ) R8VEC_SORTED_NEAREST, the index of the nearest
!    entry in the vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) r8vec_sorted_nearest
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) mid
  real ( kind = 8 ) value

  if ( n < 1 ) then
    r8vec_sorted_nearest = -1
    return
  end if

  if ( n == 1 ) then
    r8vec_sorted_nearest = 1
    return
  end if

  if ( a(1) < a(n) ) then

    if ( value < a(1) ) then
      r8vec_sorted_nearest = 1
      return
    else if ( a(n) < value ) then
      r8vec_sorted_nearest = n
      return
    end if
!
!  Seek an interval containing the value.
!
    lo = 1
    hi = n

    do while ( lo < hi - 1 )

      mid = ( lo + hi ) / 2

      if ( value == a(mid) ) then
        r8vec_sorted_nearest = mid
        return
      else if ( value < a(mid) ) then
        hi = mid
      else
        lo = mid
      end if

    end do
!
!  Take the nearest.
!
    if ( abs ( value - a(lo) ) < abs ( value - a(hi) ) ) then
      r8vec_sorted_nearest = lo
    else
      r8vec_sorted_nearest = hi
    end if

    return
!
!  A descending sorted vector A.
!
  else

    if ( value < a(n) ) then
      r8vec_sorted_nearest = n
      return
    else if ( a(1) < value ) then
      r8vec_sorted_nearest = 1
      return
    end if
!
!  Seek an interval containing the value.
!
    lo = n
    hi = 1

    do while ( lo < hi - 1 )

      mid = ( lo + hi ) / 2

      if ( value == a(mid) ) then
        r8vec_sorted_nearest = mid
        return
      else if ( value < a(mid) ) then
        hi = mid
      else
        lo = mid
      end if

    end do
!
!  Take the nearest.
!
    if ( abs ( value - a(lo) ) < abs ( value - a(hi) ) ) then
      r8vec_sorted_nearest = lo
    else
      r8vec_sorted_nearest = hi
    end if

    return

  end if

  return
end
subroutine r8vec_sorted_range ( n, r, r_lo, r_hi, i_lo, i_hi )

!*****************************************************************************80
!
!! R8VEC_SORTED_RANGE searches a sorted vector for elements in a range.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 September 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items in the vector.
!
!    real ( kind = 8 ) R(N), the sorted vector.
!
!    real ( kind = 8 ) R_LO, R_HI, the limits of the range.
!
!  Output:
!
!    integer ( kind = 4 ) I_LO, I_HI, the range of indices
!    so that I_LO <= I <= I_HI => R_LO <= R(I) <= R_HI.  If no
!    values in R lie in the range, then I_HI < I_LO will be returned.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i_hi
  integer ( kind = 4 ) i_lo
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) r_hi
  real ( kind = 8 ) r_lo
!
!  Cases we can handle immediately.
!
  if ( r(n) < r_lo ) then
    i_lo = 0
    i_hi = - 1
    return
  end if

  if ( r_hi < r(1) ) then
    i_lo = 0
    i_hi = - 1
    return
  end if
!
!  Are there are least two intervals?
!
  if ( n == 1 ) then
    if ( r_lo <= r(1) .and. r(1) <= r_hi ) then
      i_lo = 1
      i_hi = 1
    else
      i_lo = 0
      i_hi = -1
    end if
    return
  end if
!
!  Bracket R_LO.
!
  if ( r_lo <= r(1) ) then

    i_lo = 1

  else
!
!  R_LO is in one of the intervals spanned by R(J1) to R(J2).
!  Examine the intermediate interval [R(I1), R(I1+1)].
!  Does R_LO lie here, or below or above?
!
    j1 = 1
    j2 = n
    i1 = ( j1 + j2 - 1 ) / 2
    i2 = i1 + 1

    do

      if ( r_lo < r(i1) ) then
        j2 = i1
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else if ( r(i2) < r_lo ) then
        j1 = i2
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else
        i_lo = i1
        exit
      end if

    end do

  end if
!
!  Bracket R_HI
!
  if ( r(n) <= r_hi ) then

    i_hi = n

  else

    j1 = i_lo
    j2 = n
    i1 = ( j1 + j2 - 1 ) / 2
    i2 = i1 + 1

    do

      if ( r_hi < r(i1) ) then
        j2 = i1
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else if ( r(i2) < r_hi ) then
        j1 = i2
        i1 = ( j1 + j2 - 1 ) / 2
        i2 = i1 + 1
      else
        i_hi = i2
        exit
      end if

    end do

  end if
!
!  We expect to have computed the largest I_LO and smallest I_HI such that
!    R(I_LO) <= R_LO <= R_HI <= R(I_HI)
!  but what we want is actually
!    R_LO <= R(I_LO) <= R(I_HI) <= R_HI
!  which we can usually get simply by incrementing I_LO and decrementing I_HI.
!
  if ( r(i_lo) < r_lo ) then
    i_lo = i_lo + 1
    if ( n < i_lo ) then
      i_hi = i_lo - 1
    end if
  end if

  if ( r_hi < r(i_hi) ) then
    i_hi = i_hi - 1
    if ( i_hi < 1 ) then
      i_lo = i_hi + 1
    end if
  end if

  return
end
subroutine r8vec_sorted_split ( n, a, split, i_lt, i_gt )

!*****************************************************************************80
!
!! R8VEC_SORTED_SPLIT "splits" a sorted R8VEC, given a splitting value.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Given a splitting value SPLIT, the routine seeks indices
!    I_LT and I_GT so that
!
!      A(I_LT) < SPLIT < A(I_GT),
!
!    and if there are intermediate index values between I_LT and
!    I_GT, then those entries of A are exactly equal to SPLIT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    integer ( kind = 4 ) N, the number of entries in A.
!
!    real ( kind = 8 ) A(N), a sorted array.
!
!    real ( kind = 8 ) SPLIT, a value to which the entries in A are
!    to be compared.
!
!  Output:
!
!    integer ( kind = 4 ) I_LT:
!    0 if no entries are less than SPLIT;
!    N if all entries are less than SPLIT;
!    otherwise, the index of the last entry in A less than SPLIT.
!
!    integer ( kind = 4 ) I_GT:
!    1 if all entries are greater than SPLIT;
!    N+1 if no entries are greater than SPLIT;
!    otherwise the index of the first entry in A greater than SPLIT.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) hi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_gt
  integer ( kind = 4 ) i_lt
  integer ( kind = 4 ) lo
  integer ( kind = 4 ) mid
  real ( kind = 8 ) split

  if ( n < 1 ) then
    i_lt = -1
    i_gt = -1
    return
  end if

  if ( split < a(1) ) then
    i_lt = 0
    i_gt = 1
    return
  end if

  if ( a(n) < split ) then
    i_lt = n
    i_gt = n + 1
    return
  end if

  lo = 1
  hi = n

  do

    if ( lo + 1 == hi ) then
      i_lt = lo
      exit
    end if

    mid = ( lo + hi ) / 2

    if ( split <= a(mid) ) then
      hi = mid
    else
      lo = mid
    end if

  end do

  do i = i_lt + 1, n
    if ( split < a(i) ) then
      i_gt = i
      return
    end if
  end do

  i_gt = n + 1

  return
end
subroutine r8vec_sorted_undex ( x_num, x_val, x_unique_num, tol, undx, xdnu )

!*****************************************************************************80
!
!! R8VEC_SORTED_UNDEX returns unique sorted indexes for a sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The goal of this routine is to determine a vector UNDX,
!    which points, to the unique elements of X, in sorted order,
!    and a vector XDNU, which identifies, for each entry of X, the index of
!    the unique sorted element of X.
!
!    This is all done with index vectors, so that the elements of
!    X are never moved.
!
!    Assuming X is already sorted, we examine the entries of X in order,
!    noting the unique entries, creating the entries of XDNU and
!    UNDX as we go.
!
!    Once this process has been completed, the vector X could be
!    replaced by a compressed vector XU, containing the unique entries
!    of X in sorted order, using the formula
!
!      XU(I) = X(UNDX(I)).
!
!    We could then, if we wished, reconstruct the entire vector X, or
!    any element of it, by index, as follows:
!
!      X(I) = XU(XDNU(I)).
!
!    We could then replace X by the combination of XU and XDNU.
!
!    Later, when we need the I-th entry of X, we can locate it as
!    the XDNU(I)-th entry of XU.
!
!    Here is an example of a vector X, the sort and inverse sort
!    index vectors, and the unique sort and inverse unique sort vectors
!    and the compressed unique sorted vector.
!
!    Here is an example of a vector X, the unique sort and
!    inverse unique sort vectors and the compressed unique sorted vector.
!
!      I      X      XU  Undx  Xdnu
!    ----+------+------+-----+-----+
!      1 | 11.0 |  11.0    1     1
!      2 | 11.0 |  22.0    5     1
!      3 | 11.0 |  33.0    8     1
!      4 | 11.0 |  55.0    9     1
!      5 | 22.0 |                2
!      6 | 22.0 |                2
!      7 | 22.0 |                2
!      8 | 33.0 |                3
!      9 | 55.0 |
!
!    INDX(2) = 3 means that sorted item(2) is X(3).
!    XDNI(2) = 5 means that X(2) is sorted item(5).
!
!    UNDX(3) = 4 means that unique sorted item(3) is at X(4).
!    XDNU(8) = 2 means that X(8) is at unique sorted item(2).
!
!    XU(XDNU(I))) = X(I).
!    XU(I)        = X(UNDX(I)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) X_NUM, the number of data values.
!
!    real ( kind = 8 ) X_VAL(X_NUM), the data values.
!
!    integer ( kind = 4 ) X_UNIQUE_NUM, the number of unique values
!    in X_VAL.  This value is only required for languages in which the size of
!    UNDX must be known in advance.
!
!    real ( kind = 8 ) TOL, a tolerance for equality.
!
!  Output:
!
!    integer ( kind = 4 ) UNDX(X_UNIQUE_NUM), the UNDX vector.
!
!    integer ( kind = 4 ) XDNU(X_NUM), the XDNU vector.
!
  implicit none

  integer ( kind = 4 ) x_num
  integer ( kind = 4 ) x_unique_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) tol
  integer ( kind = 4 ) undx(x_unique_num)
  real ( kind = 8 ) x_val(x_num)
  integer ( kind = 4 ) xdnu(x_num)
!
!  Walk through the sorted array X.
!
  i = 1

  j = 1
  undx(j) = i

  xdnu(i) = j

  do i = 2, x_num

    if ( tol < abs ( x_val(i) - x_val(undx(j)) ) ) then
      j = j + 1
      undx(j) = i
    end if

    xdnu(i) = j

  end do

  return
end
subroutine r8vec_sorted_unique ( n, a, tol, unique_num )

!*****************************************************************************80
!
!! R8VEC_SORTED_UNIQUE keeps the unique elements in a sorted R8VEC.
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
!    29 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!    real ( kind = 8 ) A(N), the sorted array of N elements;
!
!    real ( kind = 8 ) TOL, a nonnegative tolerance for equality.
!    Set it to 0.0 for the strictest test.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the sorted unique array of UNIQUE_NUM elements.
!
!    integer ( kind = 4 ) UNIQUE_NUM, the number of unique elements
!    of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) unique_num
  real ( kind = 8 ) tol

  if ( n <= 0 ) then
    unique_num = 0
    return
  end if

  unique_num = 1

  do i = 2, n

    if ( tol < abs ( a(i) - a(unique_num) ) ) then
      unique_num = unique_num + 1
      a(unique_num) = a(i)
    end if

  end do

  return
end
subroutine r8vec_sorted_unique_count ( n, a, tol, unique_num )

!*****************************************************************************80
!
!! R8VEC_SORTED_UNIQUE_COUNT counts the unique elements in a sorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Because the array is sorted, this algorithm is O(N).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!    real ( kind = 8 ) A(N), the sorted array to examine.
!
!    real ( kind = 8 ) TOL, a nonnegative tolerance for equality.
!    Set it to 0.0 for the strictest test.
!
!  Output:
!
!    integer ( kind = 4 ) UNIQUE_NUM, the number of unique elements
!    of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) unique_num
  real ( kind = 8 ) tol

  if ( n < 1 ) then
    unique_num = 0
    return
  end if

  unique_num = 1

  do i = 2, n

    if ( tol < abs ( a(i-1) - a(i) ) ) then
      unique_num = unique_num + 1
    end if

  end do

  return
end
subroutine r8vec_sorted_unique_hist ( n, a, tol, maxuniq, unique_num, &
  auniq, acount )

!*****************************************************************************80
!
!! R8VEC_SORTED_UNIQUE_HIST histograms the unique elements of a sorted R8VEC.
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
!    09 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!    real ( kind = 8 ) A(N), the array to examine.  The elements of A
!    should have been sorted.
!
!    real ( kind = 8 ) TOL, a nonnegative tolerance for equality.
!    Set it to 0.0 for the strictest test.
!
!    integer ( kind = 4 ) MAXUNIQ, the maximum number of unique elements
!    that can be handled.  If there are more than MAXUNIQ unique
!    elements in A, the excess will be ignored.
!
!  Output:
!
!    integer ( kind = 4 ) UNIQUE_NUM, the number of unique elements
!    of A.
!
!    real ( kind = 8 ) AUNIQ(UNIQUE_NUM), the unique elements of A.
!
!    integer ( kind = 4 ) ACOUNT(UNIQUE_NUM), the number of times
!    each element of AUNIQ occurs in A.
!
  implicit none

  integer ( kind = 4 ) maxuniq
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) acount(maxuniq)
  real ( kind = 8 ) auniq(maxuniq)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) unique_num
  real ( kind = 8 ) tol
!
!  Start taking statistics.
!
  unique_num = 0

  do i = 1, n

    if ( i == 1 ) then

      unique_num = 1
      auniq(unique_num) = a(1)
      acount(unique_num) = 1

    else if ( abs ( a(i) - auniq(unique_num) ) <= tol ) then

      acount(unique_num) = acount(unique_num) + 1

    else if ( unique_num < maxuniq ) then

      unique_num = unique_num + 1
      auniq(unique_num) = a(i)
      acount(unique_num) = 1

    end if

  end do

  return
end
subroutine r8vec_split ( n, a, split, isplit )

!*****************************************************************************80
!
!! R8VEC_SPLIT "splits" an unsorted R8VEC based on a splitting value.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    If the vector is already sorted, it is simpler to do a binary search
!    on the data than to call this routine.
!
!    The vector is not assumed to be sorted before and is not
!    sorted during processing.  If sorting is not needed, then it is
!    more efficient to use this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 October 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!    real ( kind = 8 ) A(N), the array to split.
!
!    real ( kind = 8 ) SPLIT, the value used to split the vector.
!    It is not necessary that any value of A actually equal SPLIT.
!
!  Output:
!
!    real ( kind = 8 ) A(N), all the entries of A that are less than 
!    or equal to SPLIT are in A(1:ISPLIT).
!
!    integer ( kind = 4 ) ISPLIT, indicates the position of the last
!    entry of the split vector that is less than or equal to SPLIT.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) isplit
  integer ( kind = 4 ) j1
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) j3
  real ( kind = 8 ) split
!
!  Partition the vector into A1, A2, A3, where
!    A1 = A(I1:J1) holds values <= SPLIT,
!    A2 = A(I2:J2) holds untested values,
!    A3 = A(I3:J3) holds values > SPLIT.
!
  i1 = 1
  j1 = 0

  i2 = 1
  j2 = n

  i3 = n + 1
  j3 = n
!
!  Pick the next item from A2, and move it into A1 or A3.
!  Adjust indices appropriately.
!
  do i = 1, n

    if ( a(i2) <= split ) then
      i2 = i2 + 1
      j1 = j1 + 1
    else
      call r8_swap ( a(i2), a(i3-1) )
      i3 = i3 - 1
      j2 = j2 - 1
    end if

  end do

  isplit = j1

  return
end
subroutine r8vec_standardize ( n, x, xs )

!*****************************************************************************80
!
!! R8VEC_STANDARDIZE standarizes an R8VEC.
!
!  Discussion:
!
!    The output vector will have 0 mean and unit standard deviation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) X(N), the vector to be standardized.
!
!  Output:
!
!    real ( kind = 8 ) XS(N), the standardized vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) mu
  real ( kind = 8 ) sigma
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xs(n)

  call r8vec_mean ( n, x, mu )
  call r8vec_std_sample ( n, x, sigma )

  if ( sigma /= 0.0D+00 ) then
    xs(1:n) = ( x(1:n) - mu ) / sigma
  else
    xs(1:n) = 0.0D+00
  end if

  return
end
subroutine r8vec_std ( n, a, std )

!*****************************************************************************80
!
!! r8vec_std() returns the standard deviation of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The standard deviation of a vector X of length N is defined as
!
!      mean ( X(1:n) ) = sum ( X(1:n) ) / n
!
!      std ( X(1:n) ) = sqrt ( sum ( ( X(1:n) - mean )^2 ) / n
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!    N should be at least 2.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    real ( kind = 8 ) STD, the standard deviation of the vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) mean
  real ( kind = 8 ) std

  if ( n < 2 ) then

    std = 0.0D+00

  else

    mean = sum ( a(1:n) ) / real ( n, kind = 8 )

    std = sum ( ( a(1:n) - mean ) ** 2 )

    std = sqrt ( std / real ( n, kind = 8 ) )

  end if

  return
end
subroutine r8vec_std_sample ( n, a, std )

!*****************************************************************************80
!
!! r8vec_std_sample() returns the sample standard deviation of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The standard deviation of a vector X of length N is defined as
!
!      mean ( X(1:n) ) = sum ( X(1:n) ) / n
!
!      std ( X(1:n) ) = sqrt ( sum ( ( X(1:n) - mean )^2 ) / ( n - 1 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!    N should be at least 2.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    real ( kind = 8 ) STD, the sample standard deviation of the vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) mean
  real ( kind = 8 ) std

  if ( n < 2 ) then

    std = 0.0D+00

  else

    mean = sum ( a(1:n) ) / real ( n, kind = 8 )

    std = sum ( ( a(1:n) - mean ) ** 2 )

    std = sqrt ( std / real ( n - 1, kind = 8 ) )

  end if

  return
end
subroutine r8vec_std_sample_update ( nm1, mean_nm1, std_nm1, xn, n, mean_n, &
  std_n )

!*****************************************************************************80
!
!! r8vec_std_sample_update updates sample standard deviation with one new value.
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
!    06 May 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) NM1, the number of entries in the old vector.
!
!    real ( kind = 8 ) MEAN_NM1, the mean of the old vector.
!
!    real ( kind = 8 ) STD_NM1, the sample standard deviation of the old vector.
!
!    real ( kind = 8 ) XN, the new N-th entry of the vector.
!
!  Output:
!
!    integer ( kind = 4 ) N, the number of entries in the new vector.
!
!    real ( kind = 8 ) MEAN_N, the mean of the new vector.
!
!    real ( kind = 8 ) STD_N, the sample standard deviation of the new vector.
!
  implicit none

  real ( kind = 8 ) mean_n
  real ( kind = 8 ) mean_nm1
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  real ( kind = 8 ) std_n
  real ( kind = 8 ) std_nm1
  real ( kind = 8 ) xn

  if ( nm1 <= 0 ) then
    n = 1
    mean_n = xn
    std_n = 0.0D+00
  else
    n = nm1 + 1
    mean_n = mean_nm1 + ( xn - mean_nm1 ) / real ( n, kind = 8 )
    std_n = sqrt ( ( std_nm1 ** 2 * real ( nm1 - 1, kind = 8 ) &
      + ( xn - mean_nm1 ) * ( xn - mean_n ) ) / real ( n - 1, kind = 8 ) )
  end if

  return
end
subroutine r8vec_std_update ( nm1, mean_nm1, std_nm1, xn, n, mean_n, &
  std_n )

!*****************************************************************************80
!
!! r8vec_std_update updates standard deviation with one new value.
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
!    06 May 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) NM1, the number of entries in the old vector.
!
!    real ( kind = 8 ) MEAN_NM1, the mean of the old vector.
!
!    real ( kind = 8 ) STD_NM1, the standard deviation of the old vector.
!
!    real ( kind = 8 ) XN, the new N-th entry of the vector.
!
!  Output:
!
!    integer ( kind = 4 ) N, the number of entries in the new vector.
!
!    real ( kind = 8 ) MEAN_N, the mean of the new vector.
!
!    real ( kind = 8 ) STD_N, the standard deviation of the new vector.
!
  implicit none

  real ( kind = 8 ) mean_n
  real ( kind = 8 ) mean_nm1
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  real ( kind = 8 ) std_n
  real ( kind = 8 ) std_nm1
  real ( kind = 8 ) xn

  if ( nm1 <= 0 ) then
    n = 1
    mean_n = xn
    std_n = 0.0D+00
  else
    n = nm1 + 1
    mean_n = mean_nm1 + ( xn - mean_nm1 ) / real ( n, kind = 8 )
    std_n = sqrt ( ( std_nm1 ** 2 * real ( nm1, kind = 8 ) &
      + ( xn - mean_nm1 ) * ( xn - mean_n ) ) / real ( n, kind = 8 ) )
  end if

  return
end
subroutine r8vec_step ( x0, n, x, fx )

!*****************************************************************************80
!
!! R8VEC_STEP evaluates a unit step function.
!
!  Discussion:
!
!    F(X) = 0 if X < X0
!           1 if     X0 <= X
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) X0, the location of the jump.
!
!    integer ( kind = 4 ) N, the number of argument values.
!
!  Output:
!
!    real ( kind = 8 ) X(N), the arguments.
!
!    real ( kind = 8 ) FX(N), the function values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fx(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x0

  where ( x < x0 )
    fx = 0.0D+00
  else where
    fx = 1.0D+00
  end where

  return
end
subroutine r8vec_stutter ( n, a, m, am )

!*****************************************************************************80
!
!! R8VEC_STUTTER makes a "stuttering" copy of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Applying a stuttering factor M of 3, the vector A = ( 1, 5, 8 ) becomes
!    AM = ( 1, 1, 1, 5, 5, 5, 8, 8, 8 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the size of the input vector.
!
!    real ( kind = 8 ) A(N), the vector.
!
!    integer ( kind = 4 ) M, the "stuttering factor".
!
!  Output:
!
!    real ( kind = 8 ) AM(M*N), the stuttering vector.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) am(m*n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo

  do i = 1, n
    jlo = m * ( i - 1 ) + 1
    jhi = m *   i
    am(jlo:jhi) = a(i)
  end do

  return
end
function r8vec_sum ( n, a )

!*****************************************************************************80
!
!! R8VEC_SUM returns the sum of the entries of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    In FORTRAN90, this facility is offered by the built in
!    SUM function:
!
!      R8VEC_SUM ( N, A ) = SUM ( A(1:N) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), the array.
!
!  Output:
!
!    real ( kind = 8 ) R8VEC_SUM, the sum of the entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) r8vec_sum

  r8vec_sum = sum ( a(1:n) )

  return
end
subroutine r8vec_sum_running ( n, v, s )

!*****************************************************************************80
!
!! R8VEC_SUM_RUNNING computes the running sum of an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 February 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items.
!
!    real ( kind = 8 ) V(N), the data.
!
!  Output:
!
!    real ( kind = 8 ) S(N+1), the running sum.  S(I) is the sum
!    of the first I-1 values in V.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) s(n+1)
  real ( kind = 8 ) v(n)
!
!  Sum.
!
  s(1) = 0.0D+00
  do i = 2, n + 1
    s(i) = s(i-1) + v(i-1)
  end do

  return
end
subroutine r8vec_swap ( n, a1, a2 )

!*****************************************************************************80
!
!! R8VEC_SWAP swaps the entries of two R8VECs.
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
!    04 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the arrays.
!
!    real ( kind = 8 ) A1(N), A2(N), the vectors to swap.
!
!  Output:
!
!    real ( kind = 8 ) A1(N), A2(N), the swapped vectors.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  real ( kind = 8 ) a3(n)

  a3(1:n) = a1(1:n)
  a1(1:n) = a2(1:n)
  a2(1:n) = a3(1:n)

  return
end
subroutine r8vec_transpose_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_TRANSPOSE_PRINT prints an R8VEC "transposed".
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Example:
!
!    A = (/ 1.0, 2.1, 3.2, 4.3, 5.4, 6.5, 7.6, 8.7, 9.8, 10.9, 11.0 /)
!    TITLE = 'My vector:  '
!
!    My vector:  1.0    2.1    3.2    4.3    5.4
!                6.5    7.6    8.7    9.8   10.9
!               11.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of components of the vector.
!
!    real ( kind = 8 ) A(N), the vector to be printed.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) j
  character ( len = * ) title
  integer ( kind = 4 ) title_length

  title_length = len_trim ( title )

  do ilo = 1, n, 5
    if ( ilo == 1 ) then
      write ( *, '(a)', advance = 'NO' ) trim ( title )
    else
      do i = 1, title_length
        write ( *, '(1x)', advance = 'NO' )
      end do
    end if
    write ( *, '(2x)', advance = 'NO' )
    ihi = min ( ilo + 5 - 1, n )
    do j = ilo, ihi
      write ( *, '(g14.6)', advance = 'NO' ) a(j)
    end do
    write ( *, '(a)' )

  end do
 
  return
end
subroutine r8vec_undex ( x_num, x_val, x_unique_num, tol, undx, xdnu )

!*****************************************************************************80
!
!! R8VEC_UNDEX returns unique sorted indexes for an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The goal of this routine is to determine a vector UNDX,
!    which points, to the unique elements of X, in sorted order,
!    and a vector XDNU, which identifies, for each entry of X, the index of
!    the unique sorted element of X.
!
!    This is all done with index vectors, so that the elements of
!    X are never moved.
!
!    The first step of the algorithm requires the indexed sorting
!    of X, which creates arrays INDX and XDNI.  (If all the entries
!    of X are unique, then these arrays are the same as UNDX and XDNU.)
!
!    We then use INDX to examine the entries of X in sorted order,
!    noting the unique entries, creating the entries of XDNU and
!    UNDX as we go.
!
!    Once this process has been completed, the vector X could be
!    replaced by a compressed vector XU, containing the unique entries
!    of X in sorted order, using the formula
!
!      XU(1:X_UNIQUE_NUM) = X(UNDX(1:X_UNIQUE_NUM)).
!
!    We could then, if we wished, reconstruct the entire vector X, or
!    any element of it, by index, as follows:
!
!      X(I) = XU(XDNU(I)).
!
!    We could then replace X by the combination of XU and XDNU.
!
!    Later, when we need the I-th entry of X, we can locate it as
!    the XDNU(I)-th entry of XU.
!
!    Here is an example of a vector X, the sort and inverse sort
!    index vectors, and the unique sort and inverse unique sort vectors
!    and the compressed unique sorted vector.
!
!      I    X   Indx  Xdni      XU   Undx  Xdnu
!    ----+-----+-----+-----+--------+-----+-----+
!      1 | 11.     1     1 |    11,     1     1
!      2 | 22.     3     5 |    22,     2     2
!      3 | 11.     6     2 |    33,     4     1
!      4 | 33.     9     8 |    55,     5     3
!      5 | 55.     2     9 |                  4
!      6 | 11.     7     3 |                  1
!      7 | 22.     8     6 |                  2
!      8 | 22.     4     7 |                  2
!      9 | 11.     5     4 |                  1
!
!    INDX(2) = 3 means that sorted item(2) is X(3).
!    XDNI(2) = 5 means that X(2) is sorted item(5).
!
!    UNDX(3) = 4 means that unique sorted item(3) is at X(4).
!    XDNU(8) = 2 means that X(8) is at unique sorted item(2).
!
!    XU(XDNU(I))) = X(I).
!    XU(I)        = X(UNDX(I)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) X_NUM, the number of data values.
!
!    real ( kind = 8 ) X_VAL(X_NUM), the data values.
!
!    integer ( kind = 4 ) X_UNIQUE_NUM, the number of unique values
!    in X_VAL.  This value is only required for languages in which the size of
!    UNDX must be known in advance.
!
!    real ( kind = 8 ) TOL, a tolerance for equality.
!
!  Output:
!
!    integer ( kind = 4 ) UNDX(X_UNIQUE_NUM), the UNDX vector.
!
!    integer ( kind = 4 ) XDNU(X_NUM), the XDNU vector.
!
  implicit none

  integer ( kind = 4 ) x_num
  integer ( kind = 4 ) x_unique_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(x_num)
  integer ( kind = 4 ) j
  real ( kind = 8 ) tol
  integer ( kind = 4 ) undx(x_unique_num)
  real ( kind = 8 ) x_val(x_num)
  integer ( kind = 4 ) xdnu(x_num)
!
!  Implicitly sort the array.
!
  call r8vec_sort_heap_index_a ( x_num, x_val, indx )
!
!  Walk through the implicitly sorted array X.
!
  i = 1

  j = 1
  undx(j) = indx(i)

  xdnu(indx(i)) = j

  do i = 2, x_num

    if ( tol < abs ( x_val(indx(i)) - x_val(undx(j)) ) ) then
      j = j + 1
      undx(j) = indx(i)
    end if

    xdnu(indx(i)) = j

  end do

  return
end
subroutine r8vec_uniform_01 ( n, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
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
!    13 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.
!
!  Output:
!
!    real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
!    integer ( kind = 4 ) SEED, an updated seed value.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = real ( seed, kind = 8 ) * 4.656612875D-10

  end do

  return
end
subroutine r8vec_uniform_ab ( n, a, b, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_AB returns a scaled pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Each dimension ranges from A to B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    31 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A, B, the lower and upper limits.
!
!    integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.
!
!  Output:
!
!    real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
!    integer ( kind = 4 ) SEED, an updated seed value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_AB - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

  end do

  return
end
subroutine r8vec_uniform_abvec ( n, a, b, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_ABVEC returns a scaled pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Dimension I ranges from A(I) to B(I).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) A(N), B(N), the lower and upper limits
!    for each dimension.
!
!    integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.
!
!  Output:
!
!    real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
!    integer ( kind = 4 ) SEED, an updated seed value.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_ABVEC - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop 1
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + i4_huge
    end if

    r(i) = a(i) + ( b(i) - a(i) ) * real ( seed, kind = 8 ) * 4.656612875D-10

  end do

  return
end
subroutine r8vec_uniform_unit ( m, seed, w )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_UNIT generates a uniformly random unit vector.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) M, the spatial dimension.
!
!    integer ( kind = 4 ) SEED, a seed for the random number generator.
!
!  Output:
!
!    real ( kind = 8 ) W(M), a random direction vector,
!    with unit norm.
!
!    integer ( kind = 4 ) SEED, an updated seed value.
!
  implicit none

  integer ( kind = 4 ) m

  real ( kind = 8 ) norm
  integer ( kind = 4 ) seed
  real ( kind = 8 ) w(m)
!
!  Get M values from a standard normal distribution.
!
  call r8vec_normal_01 ( m, seed, w )
!
!  Compute the length of the vector.
!
  norm = sqrt ( sum ( w(1:m) ** 2 ) )
!
!  Normalize the vector.
!
  w(1:m) = w(1:m) / norm

  return
end
subroutine r8vec_unique_count ( n, a, tol, unique_num )

!*****************************************************************************80
!
!! R8VEC_UNIQUE_COUNT counts the unique elements in an unsorted R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    Because the array is unsorted, this algorithm is O(N^2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!    real ( kind = 8 ) A(N), the unsorted array to examine.
!
!    real ( kind = 8 ) TOL, a nonnegative tolerance for equality.
!    Set it to 0.0 for the strictest test.
!
!  Output:
!
!    integer ( kind = 4 ) UNIQUE_NUM, the number of unique elements
!    of A.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) unique_num
  real ( kind = 8 ) tol

  unique_num = 0

  do i = 1, n

    unique_num = unique_num + 1

    do j = 1, i - 1

      if ( abs ( a(i) - a(j) ) <= tol ) then
        unique_num = unique_num - 1
        exit
      end if

    end do

  end do

  return
end
subroutine r8vec_unique_index ( n, a, tol, unique_index )

!*****************************************************************************80
!
!! R8VEC_UNIQUE_INDEX indexes the unique occurrence of values in an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    For element A(I) of the vector, UNIQUE_INDEX(I) is the uniqueness index
!    of A(I).  That is, if A_UNIQUE contains the unique elements of A,
!    gathered in order, then
!
!      A_UNIQUE ( UNIQUE_INDEX(I) ) = A(I)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2008
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of elements of A.
!
!    real ( kind = 8 ) A(N), the array.
!
!    real ( kind = 8 ) TOL, a tolerance for equality.
!
!  Output:
!
!    integer ( kind = 4 ) UNIQUE_INDEX(N), the unique index.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) tol
  integer ( kind = 4 ) unique_index(n)
  integer ( kind = 4 ) unique_num

  unique_index(1:n) = -1
  unique_num = 0

  do i = 1, n

    if ( unique_index(i) == -1 ) then

      unique_num = unique_num + 1
      unique_index(i) = unique_num

      do j = i + 1, n
        if ( abs ( a(i) - a(j) ) <= tol ) then
          unique_index(j) = unique_num
        end if
      end do

    end if

  end do

  return
end
subroutine r8vec_variance ( n, a, variance )

!*****************************************************************************80
!
!! r8vec_variance() returns the variance of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The variance of a vector X of length N is defined as
!
!      mean ( X(1:n) ) = sum ( X(1:n) ) / n
!
!      var ( X(1:n) ) = sum ( ( X(1:n) - mean )^2 ) / ( n )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 201
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!    N should be at least 2.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    real ( kind = 8 ) VARIANCE, the variance of the vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) mean
  real ( kind = 8 ) variance

  if ( n < 2 ) then

    variance = 0.0D+00

  else

    mean = sum ( a(1:n) ) / real ( n, kind = 8 )

    variance = sum ( ( a(1:n) - mean ) ** 2 )

    variance = variance / real ( n, kind = 8 )

  end if

  return
end
subroutine r8vec_variance_circular ( n, x, variance_circular )

!*****************************************************************************80
!
!! R8VEC_VARIANCE_CIRCULAR returns the circular variance of an R8VEC.
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
!    02 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!    real ( kind = 8 ) X(N), the vector whose variance is desired.
!
!  Output:
!
!    real ( kind = 8 ) VARIANCE_CIRCULAR, the circular variance
!    of the vector entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) mean
  real ( kind = 8 ) variance_circular
  real ( kind = 8 ) x(n)

  call r8vec_mean ( n, x, mean )

  variance_circular = &
      ( sum ( cos ( x(1:n) - mean ) ) ) ** 2 &
    + ( sum ( sin ( x(1:n) - mean ) ) ) ** 2

  variance_circular = sqrt ( variance_circular ) / real ( n, kind = 8 )

  variance_circular = 1.0D+00 - variance_circular

  return
end
subroutine r8vec_variance_sample ( n, a, variance )

!*****************************************************************************80
!
!! r8vec_variance_sample() returns the sample_variance of an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The variance of a vector X of length N is defined as
!
!      mean ( X(1:n) ) = sum ( X(1:n) ) / n
!
!      var ( X(1:n) ) = sum ( ( X(1:n) - mean )^2 ) / ( n - 1 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!    N should be at least 2.
!
!    real ( kind = 8 ) A(N), the vector.
!
!  Output:
!
!    real ( kind = 8 ) VARIANCE, the sample variance of the vector.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) mean
  real ( kind = 8 ) variance

  if ( n < 2 ) then

    variance = 0.0D+00

  else

    mean = sum ( a(1:n) ) / real ( n, kind = 8 )

    variance = sum ( ( a(1:n) - mean ) ** 2 )

    variance = variance / real ( n - 1, kind = 8 )

  end if

  return
end
subroutine r8vec_variance_sample_update ( nm1, mean_nm1, variance_nm1, xn, n, &
  mean_n, variance_n )

!*****************************************************************************80
!
!! r8vec_variance_sample_update() updates the sample variance with one new value.
!
!  Discussion:
!
!    On first call:
!      nm1 = 0
!      mean_nm1 = 0.0
!      variance_nm1 = 0.0
!      xn = first value to be handled.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2021
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    John D Cook,
!    Accurately computing running variance,
!    https://www.johndcook.com/blog/standard_deviation/
!
!  Input:
!
!    integer ( kind = 4 ) NM1, the number of entries in the old vector.
!
!    real ( kind = 8 ) MEAN_NM1, the mean of the old vector.
!
!    real ( kind = 8 ) VARIANCE_NM1, the variance of the old vector.
!
!    real ( kind = 8 ) XN, the new N-th entry of the vector.
!
!  Output:
!
!    integer ( kind = 4 ) N, the number of entries in the new vector.
!
!    real ( kind = 8 ) MEAN_N, the mean of the new vector.
!
!    real ( kind = 8 ) VARIANCE_N, the variance of the new vector.
!
  implicit none

  real ( kind = 8 ) mean_n
  real ( kind = 8 ) mean_nm1
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  real ( kind = 8 ) variance_n
  real ( kind = 8 ) variance_nm1
  real ( kind = 8 ) xn

  if ( nm1 <= 0 ) then
    n = 1
    mean_n = xn
    variance_n = 0.0D+00
  else
    n = nm1 + 1
    mean_n = mean_nm1 + ( xn - mean_nm1 ) / real ( n, kind = 8 )
    variance_n = ( variance_nm1 * real ( nm1 - 1, kind = 8 ) &
      + ( xn - mean_nm1 ) * ( xn - mean_n ) ) / real ( n - 1, kind = 8 )
  end if

  return
end
subroutine r8vec_variance_update ( nm1, mean_nm1, variance_nm1, xn, n, &
  mean_n, variance_n )

!*****************************************************************************80
!
!! r8vec_variance_update() updates the variance with one new value.
!
!  Discussion:
!
!    On first call:
!      nm1 = 0
!      mean_nm1 = 0.0
!      variance_nm1 = 0.0
!      xn = first value to be handled.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 May 2021
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    John D Cook,
!    Accurately computing running variance,
!    https://www.johndcook.com/blog/standard_deviation/
!
!  Input:
!
!    integer ( kind = 4 ) NM1, the number of entries in the old vector.
!
!    real ( kind = 8 ) MEAN_NM1, the mean of the old vector.
!
!    real ( kind = 8 ) VARIANCE_NM1, the variance of the old vector.
!
!    real ( kind = 8 ) XN, the new N-th entry of the vector.
!
!  Output:
!
!    integer ( kind = 4 ) N, the number of entries in the new vector.
!
!    real ( kind = 8 ) MEAN_N, the mean of the new vector.
!
!    real ( kind = 8 ) VARIANCE_N, the variance of the new vector.
!
  implicit none

  real ( kind = 8 ) mean_n
  real ( kind = 8 ) mean_nm1
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nm1
  real ( kind = 8 ) variance_n
  real ( kind = 8 ) variance_nm1
  real ( kind = 8 ) xn

  if ( nm1 <= 0 ) then
    n = 1
    mean_n = xn
    variance_n = 0.0D+00
  else
    n = nm1 + 1
    mean_n = mean_nm1 + ( xn - mean_nm1 ) / real ( n, kind = 8 )
    variance_n = ( variance_nm1 * real ( nm1, kind = 8 ) &
      + ( xn - mean_nm1 ) * ( xn - mean_n ) ) / real ( n, kind = 8 )
  end if

  return
end
subroutine r8vec_vector_triple_product ( v1, v2, v3, v )

!*****************************************************************************80
!
!! R8VEC_VECTOR_TRIPLE_PRODUCT computes the vector triple product.
!
!  Discussion:
!
!    VTRIPLE = V1 x ( V2 x V3 )
!
!    VTRIPLE is a vector perpendicular to V1, lying in the plane
!    spanned by V2 and V3.  The norm of VTRIPLE is the product
!    of the norms of V1, V2 and V3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2010
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) V1(3), V2(3), V3(3), three vectors.
!
!  Output:
!
!    real ( kind = 8 ) V(3), the vector triple product.
!
  implicit none

  real ( kind = 8 ) v(3)
  real ( kind = 8 ) v1(3)
  real ( kind = 8 ) v2(3)
  real ( kind = 8 ) v3(3)
  real ( kind = 8 ) v4(3)

  call r8vec_cross_product_3d ( v2, v3, v4 )

  call r8vec_cross_product_3d ( v1, v4, v )

  return
end
subroutine r8vec_write ( n, r, output_file )

!*****************************************************************************80
!
!! R8VEC_WRITE writes an R8VEC to a file.
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
!    31 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the order of the matrix.
!
!    real ( kind = 8 ) R(N), the vector to be written.
!
!    character ( len = * ) OUTPUT_FILE, the name of the file to which
!    the information is to be written.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  character ( len = * ) output_file
  integer ( kind = 4 ) output_unit
  real ( kind = 8 ) r(n)

  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_file, status = 'replace' )

  do i = 1, n
    write ( output_unit, '(2x,g16.8)' ) r(i)
  end do

  close ( unit = output_unit )

  return
end
subroutine r8vec_zeros ( n, a )

!*****************************************************************************80
!
!! r8vec_zeros zeroes out an R8VEC.
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
!    10 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the vector.
!
!  Output:
!
!    real ( kind = 8 ) A(N), the vector to be zeroed.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)

  a(1:n) = 0.0D+00

  return
end
subroutine r8vec2_compare ( n, a1, a2, i, j, isgn )

!*****************************************************************************80
!
!! r8vec2_compare compares two entries in an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    The lexicographic ordering is used.
!
!  Example:
!
!    A1(I) A2(I)   A1(J) A2(J)  ISGN
!    -----------   -----------  ----
!    1.0   5.0  <  1.0   6.0     -1
!    1.0   5.0  <  2.0   8.0     -1
!    1.0   5.0  <  9.0   1.0     -1
!    1.0   5.0  =  1.0   5.0      0
!    1.0   5.0  >  0.0   2.0     +1
!    1.0   5.0  >  0.0   5.0     +1
!    1.0   5.0  >  1.0   3.0     +1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of data items.
!
!    real ( kind = 8 ) A1(N), A2(N), the two components of each item.
!
!    integer ( kind = 4 ) I, J, the items to be compared.
!
!  Output:
!
!    integer ( kind = 4 ) ISGN, the results of the comparison:
!    -1, item I < item J,
!     0, item I = item J,
!    +1, item I > item J.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j

  isgn = 0

       if ( a1(i) < a1(j) ) then

    isgn = -1

  else if ( a1(i) == a1(j) ) then

         if ( a2(i) < a2(j) ) then
      isgn = -1
    else if ( a2(i) < a2(j) ) then
      isgn = 0
    else if ( a2(j) < a2(i) ) then
      isgn = +1
    end if

  else if ( a1(j) < a1(i) ) then

    isgn = +1

  end if

  return
end
subroutine r8vec2_print ( n, a1, a2, title )

!*****************************************************************************80
!
!! r8vec2_print prints an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of components of the vector.
!
!    real ( kind = 8 ) A1(N), A2(N), the vectors to be printed.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, a1(i), a2(i)
  end do

  return
end
subroutine r8vec2_print_some ( n, x1, x2, max_print, title )

!*****************************************************************************80
!
!! r8vec2_print_some() prints "some" of an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vectors, is no more than MAX_PRINT, then
!    the entire vectors are printed, one entry of each per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries of the vectors.
!
!    real ( kind = 8 ) X1(N), X2(N), the vectors.
!
!    integer ( kind = 4 ) MAX_PRINT, the maximum number of lines to print.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) max_print
  character ( len = * ) title
  real ( kind = 8 ) x1(n)
  real ( kind = 8 ) x2(n)

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) i, x1(i), x2(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) i, x1(i), x2(i)
    end do
    write ( *, '(a)' ) '  ......  ..............  ..............'
    i = n
    write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) i, x1(i), x2(i)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) i, x1(i), x2(i)
    end do
    i = max_print
    write ( *, '(2x,i8,2x,g14.6,2x,g14.6,2x,a)' ) i, x1(i), x2(i), &
      '...more entries...'

  end if

  return
end
subroutine r8vec2_sort_a ( n, a1, a2 )

!*****************************************************************************80
!
!! r8vec2_sort_a ascending sorts an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    Each item to be sorted is a pair (I,J), with the I
!    and J values stored in separate vectors A1 and A2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items of data.
!
!    real ( kind = 8 ) A1(N), A2(N), the data to be sorted.
!
!  Output:
!
!    real ( kind = 8 ) A1(N), A2(N), the sorted data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j

  if ( n <= 1 ) then
    return
  end if
!
!  Initialize.
!
  i = 0
  indx = 0
  isgn = 0
  j = 0
!
!  Call the external heap sorter.
!
  do

    call sort_heap_external ( n, indx, i, j, isgn )
!
!  Interchange the I and J objects.
!
    if ( 0 < indx ) then

      call r8_swap ( a1(i), a1(j) )
      call r8_swap ( a2(i), a2(j) )
!
!  Compare the I and J objects.
!
    else if ( indx < 0 ) then

      call r8vec2_compare ( n, a1, a2, i, j, isgn )

    else if ( indx == 0 ) then

      exit

    end if

  end do

  return
end
subroutine r8vec2_sort_d ( n, a1, a2 )

!*****************************************************************************80
!
!! r8vec2_sort_d descending sorts an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    Each item to be sorted is a pair (I,J), with the I
!    and J values stored in separate vectors A1 and A2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items of data.
!
!    real ( kind = 8 ) A1(N), A2(N), the data to be sorted.
!
!  Output:
!
!    real ( kind = 8 ) A1(N), A2(N), the sorted data.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j

  if ( n <= 1 ) then
    return
  end if
!
!  Initialize.
!
  i = 0
  indx = 0
  isgn = 0
  j = 0
!
!  Call the external heap sorter.
!
  do

    call sort_heap_external ( n, indx, i, j, isgn )
!
!  Interchange the I and J objects.
!
    if ( 0 < indx ) then

      call r8_swap ( a1(i), a1(j) )
      call r8_swap ( a2(i), a2(j) )
!
!  Compare the I and J objects.
!  Reverse the value of ISGN to effect a descending sort.
!
    else if ( indx < 0 ) then

      call r8vec2_compare ( n, a1, a2, i, j, isgn )

      isgn = -isgn

    else if ( indx == 0 ) then

      exit

    end if

  end do

  return
end
subroutine r8vec2_sort_heap_index_a ( n, x, y, indx )

!*****************************************************************************80
!
!! r8vec2_sort_heap_index_a does an indexed heap ascending sort of an R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    The sorting is not actually carried out.  Rather an index array is
!    created which defines the sorting.  This array may be used to sort
!    or index the array, or to sort or index related arrays keyed on the
!    original array.
!
!    ( X(I), Y(I) ) < ( X(J), Y(J) ) if:
!
!    * X(I) < X(J), or
!
!    * X(I) = X(J), and Y(I) < Y(J).
!
!    Once the index array is computed, the sorting can be carried out
!    "implicitly:
!
!      ( X(INDX(1:N)), Y(INDX(1:N) ), is sorted,
!
!    or explicitly, by the call
!
!      call r8vec_permute ( n, indx, x )
!      call r8vec_permute ( n, indx, y )
!
!    after which ( X(1:N), Y(1:N) ), is sorted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) X(N),Y(N), pairs of X, Y coordinates of points.
!
!  Output:
!
!    integer ( kind = 4 ) INDX(N), the sort index.  The
!    I-th element of the sorted array has coordinates ( X(INDX(I)), Y(INDX(I) ).
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) indxt
  integer ( kind = 4 ) ir
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xval
  real ( kind = 8 ) y(n)
  real ( kind = 8 ) yval

  if ( n < 1 ) then
    return
  end if

  call i4vec_indicator1 ( n, indx )

  if ( n == 1 ) then
    return
  end if

  l = n / 2 + 1
  ir = n

  do

    if ( 1 < l ) then

      l = l - 1
      indxt = indx(l)
      xval = x(indxt)
      yval = y(indxt)

    else

      indxt = indx(ir)
      xval = x(indxt)
      yval = y(indxt)
      indx(ir) = indx(1)
      ir = ir - 1

      if ( ir == 1 ) then
        indx(1) = indxt
        exit
      end if

    end if

    i = l
    j = l + l

    do while ( j <= ir )

      if ( j < ir ) then

        if ( x(indx(j)) < x(indx(j+1)) .or. &
          ( x(indx(j)) == x(indx(j+1)) .and. y(indx(j)) < y(indx(j+1)) ) ) then
          j = j + 1
        end if

      end if

      if ( xval < x(indx(j)) .or. &
          ( xval == x(indx(j)) .and. yval < y(indx(j)) ) ) then
        indx(i) = indx(j)
        i = j
        j = j + j
      else
        j = ir + 1
      end if

    end do

    indx(i) = indxt

  end do

  return
end
subroutine r8vec2_sorted_unique ( n, a1, a2, unique_num )

!*****************************************************************************80
!
!! R8VEC2_SORTED_UNIQUE keeps unique elements in a sorted R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    Item I is stored as the pair A1(I), A2(I).
!
!    The items must have been sorted, or at least it must be the
!    case that equal items are stored in adjacent vector locations.
!
!    If the items were not sorted, then this routine will only
!    replace a string of equal values by a single representative.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items.
!
!    real ( kind = 8 ) A1(N), A2(N), the data.
!
!  Output:
!
!    real ( kind = 8 ) A1(N), A2(N), the first UNIQUE_NUM entries
!    contain the unique values.
!
!    integer ( kind = 4 ) UNIQUE_NUM, the number of unique items.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) itest
  integer ( kind = 4 ) unique_num

  if ( n <= 0 ) then
    unique_num = 0
    return
  end if

  unique_num = 1

  do itest = 2, n

    if ( a1(itest) /= a1(unique_num) .or. a2(itest) /= a2(unique_num) ) then

      unique_num = unique_num + 1

      a1(unique_num) = a1(itest)
      a2(unique_num) = a2(itest)

    end if

  end do

  return
end
subroutine r8vec2_sorted_unique_index ( n, a1, a2, unique_num, indx )

!*****************************************************************************80
!
!! r8vec2_sorted_unique_index indexes unique elements in a sorted R8VEC2.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!    Item I is stored as the pair A1(I), A2(I).
!
!    The items must have been sorted, or at least it should be the
!    case that equal items are stored in adjacent vector locations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items.
!
!    real ( kind = 8 ) A1(N), A2(N), the array of N items.
!
!  Output:
!
!    integer ( kind = 4 ) UNIQUE_NUM, the number of unique items.
!
!    integer ( kind = 4 ) INDX(N), contains in entries 1 through
!    UNIQUE_NUM an index array of the unique items.  To build new arrays
!    with no repeated elements:
!      B1(1:UNIQUE_NUM) = A1(INDX(1:UNIQUE_NUM))
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  integer ( kind = 4 ) indx(n)
  integer ( kind = 4 ) itest
  integer ( kind = 4 ) unique_num

  if ( n <= 0 ) then
    unique_num = 0
    return
  end if

  unique_num = 1
  indx(1) = 1

  do itest = 2, n

    if ( a1(itest-1) /= a1(itest) .or. a2(itest-1) /= a2(itest) ) then

      unique_num = unique_num + 1

      indx(unique_num) = itest

    end if

  end do

  indx(unique_num+1:n) = 0

  return
end
subroutine r8vec2_sum_max_index ( n, a, b, sum_max_index )

!*****************************************************************************80
!
!! r8vec2_sum_max_index returns the index of the maximum sum of two R8VEC's.
!
!  Discussion:
!
!    An R8VEC2 is a dataset consisting of N pairs of R8's, stored
!    as two separate vectors A1 and A2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of entries in the array.
!
!    real ( kind = 8 ) A(N), B(N), two arrays whose sum
!    is to be examined.
!
!  Output:
!
!    integer ( kind = 4 ) SUM_MAX_INDEX, the index of the largest A+B.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) b(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) sum_max
  integer ( kind = 4 ) sum_max_index

  if ( n <= 0 ) then

    sum_max_index = -1

  else

    sum_max_index = 1
    sum_max = a(1) + b(1)

    do i = 2, n
      if ( sum_max < a(i) + b(i) ) then
        sum_max = a(i) + b(i)
        sum_max_index = i
      end if
    end do

  end if

  return
end
subroutine r8vec3_print ( n, a1, a2, a3, title )

!*****************************************************************************80
!
!! r8vec3_print prints an R8VEC3.
!
!  Discussion:
!
!    An R8VEC3 is a dataset consisting of N triples of R8's, stored
!    as three separate vectors A1, A2, A3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of components of the vector.
!
!    real ( kind = 8 ) A1(N), A2(N), A3(N), the vectors to be printed.
!
!    character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a1(n)
  real ( kind = 8 ) a2(n)
  real ( kind = 8 ) a3(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  do i = 1, n
    write ( *, '(i8,3g14.6)' ) i, a1(i), a2(i), a3(i)
  end do

  return
end
subroutine sort_heap_external ( n, indx, i, j, isgn )

!*****************************************************************************80
!
!! sort_heap_external externally sorts a list of items into ascending order.
!
!  Discussion:
!
!    The actual list of data is not passed to the routine.  Hence this
!    routine may be used to sort integers, reals, numbers, names,
!    dates, shoe sizes, and so on.  After each call, the routine asks
!    the user to compare or interchange two items, until a special
!    return value signals that the sorting is completed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2004
!
!  Author:
!
!    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978,
!    ISBN: 0-12-519260-6,
!    LC: QA164.N54.
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of items to be sorted.
!
!    integer ( kind = 4 ) INDX, the main communication signal.
!    The user must set INDX to 0 before the first call.
!    Thereafter, the user should not change the value of INDX until
!    the sorting is done.
!
!    integer ( kind = 4 ) ISGN, results of comparison of elements
!    I and J. (Used only when the previous call returned INDX less than 0).
!    ISGN <= 0 means I is less than or equal to J;
!    0 <= ISGN means I is greater than or equal to J.
!
!  Output:
!
!    integer ( kind = 4 ) INDX, the main communication signal.
!    On return, if INDX is
!    * greater than 0,
!      > interchange items I and J;
!      > call again.
!    * less than 0,
!      > compare items I and J;
!      > set ISGN = -1 if I < J, ISGN = +1 if J < I;
!      > call again.
!    * equal to 0, the sorting is done.
!
!    integer ( kind = 4 ) I, J, the indices of two items.
!    On return with INDX positive, elements I and J should be interchanged.
!    On return with INDX negative, elements I and J should be compared, and
!    the result reported in ISGN on the next call.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ), save :: i_save = 0
  integer ( kind = 4 ) indx
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) j
  integer ( kind = 4 ), save :: j_save = 0
  integer ( kind = 4 ), save :: k = 0
  integer ( kind = 4 ), save :: k1 = 0
  integer ( kind = 4 ) n
  integer ( kind = 4 ), save :: n1 = 0
!
!  INDX = 0: This is the first call.
!
  if ( indx == 0 ) then

    i_save = 0
    j_save = 0
    k = n / 2
    k1 = k
    n1 = n
!
!  INDX < 0: The user is returning the results of a comparison.
!
  else if ( indx < 0 ) then

    if ( indx == -2 ) then

      if ( isgn < 0 ) then
        i_save = i_save + 1
      end if

      j_save = k1
      k1 = i_save
      indx = -1
      i = i_save
      j = j_save
      return

    end if

    if ( 0 < isgn ) then
      indx = 2
      i = i_save
      j = j_save
      return
    end if

    if ( k <= 1 ) then

      if ( n1 == 1 ) then
        i_save = 0
        j_save = 0
        indx = 0
      else
        i_save = n1
        n1 = n1 - 1
        j_save = 1
        indx = 1
      end if

      i = i_save
      j = j_save
      return

    end if

    k = k - 1
    k1 = k
!
!  0 < INDX, the user was asked to make an interchange.
!
  else if ( indx == 1 ) then

    k1 = k

  end if

  do

    i_save = 2 * k1

    if ( i_save == n1 ) then
      j_save = k1
      k1 = i_save
      indx = -1
      i = i_save
      j = j_save
      return
    else if ( i_save <= n1 ) then
      j_save = i_save + 1
      indx = -2
      i = i_save
      j = j_save
      return
    end if

    if ( k <= 1 ) then
      exit
    end if

    k = k - 1
    k1 = k

  end do

  if ( n1 == 1 ) then
    i_save = 0
    j_save = 0
    indx = 0
    i = i_save
    j = j_save
  else
    i_save = n1
    n1 = n1 - 1
    j_save = 1
    indx = 1
    i = i_save
    j = j_save
  end if

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! timestamp prints the current YMDHMS date as a time stamp.
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
