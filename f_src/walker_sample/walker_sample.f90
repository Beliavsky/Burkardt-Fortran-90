function i4_choose ( n, k )

!*****************************************************************************80
!
!! I4_CHOOSE computes the binomial coefficient C(N,K) as an I4.
!
!  Discussion:
!
!    The value is calculated in such a way as to avoid overflow and
!    roundoff.  The calculation is done in integer arithmetic.
!
!    The formula used is:
!
!      C(N,K) = N! / ( K! * (N-K)! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    ML Wolfson, HV Wright,
!    Algorithm 160:
!    Combinatorial of M Things Taken N at a Time,
!    Communications of the ACM,
!    Volume 6, Number 4, April 1963, page 161.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, K, are the values of N and K.
!
!    Output, integer ( kind = 4 ) I4_CHOOSE, the number of combinations of N
!    things taken K at a time.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) k
  integer ( kind = 4 ) mn
  integer ( kind = 4 ) mx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) value

  mn = min ( k, n - k )
  mx = max ( k, n - k )

  if ( mn < 0 ) then

    value = 0

  else if ( mn == 0 ) then

    value = 1

  else

    value = mx + 1

    do i = 2, mn
      value = ( value * ( mx + i ) ) / i
    end do

  end if

  i4_choose = value

  return
end
subroutine i4_choose_test ( )

!*****************************************************************************80
!
!! I4_CHOOSE_TEST tests I4_CHOOSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) cnk
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_CHOOSE_TEST'
  write ( *, '(a)' ) '  I4_CHOOSE evaluates C(N,K).'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         N         K       CNK'
 
  do n = 0, 4
    write ( *, '(a)' ) ' '
    do k = 0, n
      cnk = i4_choose ( n, k )
      write ( *, '(2x,i8,2x,i8,2x,i8)' ) n, k, cnk
    end do
  end do
 
  return
end
function i4_uniform_ab ( a, b, seed )

!*****************************************************************************80
!
!! I4_UNIFORM_AB returns a scaled pseudorandom I4 between A and B.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, integer ( kind = 4 ) I4_UNIFORM_AB, a number between A and B.
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
subroutine i4_uniform_ab_test ( )

!*****************************************************************************80
!
!! I4_UNIFORM_AB_TEST tests I4_UNIFORM_AB.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: a = -100
  integer ( kind = 4 ), parameter :: b = 200
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'I4_UNIFORM_AB_TEST'
  write ( *, '(a)' ) '  I4_UNIFORM_AB computes pseudorandom values '
  write ( *, '(a)' ) '  in an interval [A,B].'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  The lower endpoint A = ', a
  write ( *, '(a,i12)' ) '  The upper endpoint B = ', b
  write ( *, '(a,i12)' ) '  The initial seed is ', seed
  write ( *, '(a)' ) ' '

  do i = 1, 20

    j = i4_uniform_ab ( a, b, seed )

    write ( *, '(2x,i8,2x,i8)' ) i, j

  end do

  return
end
subroutine normalize ( n, x )

!*****************************************************************************80
!
!! NORMALIZE scales a vector X so its entries sum to 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    Original C version by Warren Smith.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, indicates the size of X.
!
!    Input/output, real ( kind = 8 ) X(0:N+1), the vector to be normalized.
!    Entries X(1) through X(N) will sum to 1 on output.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) s
  real ( kind = 8 ) x(0:n+1)

  s = sum ( abs ( x(1:n) ) )
  x(1:n) = x(1:n) / s

  return
end
subroutine normalize_test ( )

!*****************************************************************************80
!
!! NORMALIZE_TEST tests NORMALIZE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ) x_norm

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMALIZE_TEST'
  write ( *, '(a)' ) '  NORMALIZE normalizes entries 1 through N of a vector'
  write ( *, '(a)' ) '  of length N+2.'

  n = 5
  seed = 123456789
  allocate ( x(0:n+1) )
  call r8vec_uniform_01 ( n + 2, seed, x )
  call r8vec_print ( n + 2, x, '  Initial X:' )

  x_norm = sum ( abs ( x(1:n) ) )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Initial L1 norm of X(1:N) = ', x_norm

  call normalize ( n, x )

  call r8vec_print ( n + 2, x, '  Normalized X:' )

  x_norm = sum ( abs ( x(1:n) ) )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Final L1 norm of X(1:N) = ', x_norm
!
!  Free memory.
!
  deallocate ( x )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'NORMALIZE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'

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
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0. On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
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
subroutine r8_uniform_01_test ( )

!*****************************************************************************80
!
!! R8_UNIFORM_01_TEST tests R8_UNIFORM_01
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
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  integer ( kind = 4 ), parameter :: n = 1000
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) variance
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  R8_UNIFORM_01 samples a uniform random'
  write ( *, '(a)' ) '  distribution in [0,1].'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Starting with seed = ', seed

  do i = 1, n
    x(i) = r8_uniform_01 ( seed )
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  First few values:'
  write ( *, '(a)' ) ' '
  do i = 1, 5
    write ( *, '(2x,i8,2x,g14.6)' ) i, x(i)
  end do

  mean = 0.0D+00
  do i = 1, n
    mean = mean + x(i)
  end do
  mean = mean / real ( n, kind = 8 )
 
  variance = 0.0D+00
  do i = 1, n
    variance = variance + ( x(i) - mean ) ** 2
  end do
  variance = variance / real ( n, kind = 8 )

  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of values computed was N = ', n
  write ( *, '(a,g14.6)' ) '  Average value was ', mean
  write ( *, '(a,g14.6)' ) '  Minimum value was ', minval ( x(1:n) )
  write ( *, '(a,g14.6)' ) '  Maximum value was ', maxval ( x(1:n) )
  write ( *, '(a,g14.6)' ) '  Variance was ', variance

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
subroutine r8vec_print_test ( )

!*****************************************************************************80
!
!! R8VEC_PRINT_TEST tests R8VEC_PRINT.
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
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ), dimension ( n ) :: a = (/ &
    123.456D+00, 0.000005D+00, -1.0D+06, 3.14159265D+00 /)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_PRINT_TEST'
  write ( *, '(a)' ) '  R8VEC_PRINT prints an R8VEC.'

  call r8vec_print ( n, a, '  The R8VEC:' )

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
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
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
subroutine r8vec_uniform_01_test ( )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01_TEST tests R8VEC_UNIFORM_01.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 20

  real ( kind = 8 ) r(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_UNIFORM_01_TEST'
  write ( *, '(a)' ) '  R8VEC_UNIFORM_01 returns a random R8VEC '
  write ( *, '(a)' ) '  with entries in [0,1].'

  seed = 123456789

  write ( *, '(a)' ) ' '
  write ( *, '(a,i12)' ) '  Input SEED = ', seed

  call r8vec_uniform_01 ( n, seed, r )

  call r8vec_print ( n, r, '  Random R8VEC:' )

  return
end
subroutine random_permutation ( n, x, seed )

!*****************************************************************************80
!
!! RANDOM_PERMUTATION applies a random permutation to an array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    Original C version by Warren Smith.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, indicates the size of X.
!
!    Input/output, real ( kind = 8 ) X(0:N+1).  On output, entries 
!    X(1) through X(N) have been randomly permuted.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t
  real ( kind = 8 ) x(0:n+1)

  do i = 1, n - 1

    j = i4_uniform_ab ( i, n, seed )

    t    = x(i)
    x(i) = x(j)
    x(j) = t 
     
  end do

  return
end
subroutine random_permutation_test ( )

!*****************************************************************************80
!
!! RANDOM_PERMUTATION_TEST tests RANDOM_PERMUTATION.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RANDOM_PERMUTATION_TEST'
  write ( *, '(a)' ) '  RANDOM_PERMUTATION randomly permutes entries 1 through'
  write ( *, '(a)' ) '  N of a vector X[0:N+1].'

  n = 5
  allocate ( x(0:n+1) )
  do i = 0, n + 1
    x(i) = real ( i, kind = 8 )
  end do
  seed = 123456789
  call r8vec_print ( n + 2, x, '  Initial X:' )
  call random_permutation ( n, x, seed )
  call r8vec_print ( n + 2, x, '  Permuted X:' )
!
!  Free memory.
!
  deallocate ( x )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'RANDOM_PERMUTATION_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'

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
subroutine walker_build ( n, x, y, a )

!*****************************************************************************80
!
!! WALKER_BUILD sets up the data for a Walker sampler.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    Original C version by Warren Smith.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Warren Smith,
!    How to sample from a probability distribution,
!    April 18, 2002.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, indicates the size of X.
!
!    Input, real ( kind = 8 ) X(0:N+1), contains in X(1) through X(N) the
!    probabilities of outcomes 1 through N.  
!
!    Output, real ( kind = 8 ) Y(0:N+1), the Walker threshold vector.
!
!    Output, integer ( kind = 4 ) A(0:N+1), the Walker index vector.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(0:n+1)
  integer ( kind = 4 ) b(0:n+1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) x(0:n+1)
  real ( kind = 8 ) y(0:n+1)
!
!  Initialize A.
!
  do i = 0, n + 1
    a(i) = i
  end do
!
!  Initialize B to the "stay here" value, and set sentinel values at the ends.
!
  do i = 0, n + 1
    b(i) = i
  end do
!
!  Copy Y from X.
!  Scale the probability vector and set sentinel values at the ends.
!
  y(0) = 0.0D+00
  y(1:n) = x(1:n) * real ( n, kind = 8 )
  y(n+1) = 2.0D+00

  i = 0
  j = n + 1

  do
!
!  Find i so Y(B(i)) needs more.
!
    do  
      i = i + 1
      if ( 1.0D+00 <= y(b(i)) ) then
        exit
      end if
    end do
!
!  Find j so Y(B(j)) wants less.
!
    do
      j = j - 1
      if ( y(b(j)) < 1.0D+00 ) then
        exit
      end if
    end do

    if ( j <= i ) then
      exit
    end if
!
!  Swap B(i) and B(j).
!
    k    = b(i)
    b(i) = b(j)
    b(j) = k

  end do

  i = j
  j = j + 1

  do while ( 0 < i )
!
!  Find J such that Y(B(j)) needs more.
!
    do while ( y(b(j)) <= 1.0 )
      j = j + 1
    end do
!
!  Meanwhile, Y(B(i)) wants less.
!
    if ( n < j ) then
      exit
    end if
!
!  B(i) will donate to B(j) to fix up.
!
    y(b(j)) = y(b(j)) - ( 1.0D+00 - y(b(i)) )     
    a(b(i)) = b(j)             
! 
!  Y(B(j)) now wants less so readjust ordering.
!
    if ( y(b(j)) < 1.0D+00 ) then

      k    = b(i)
      b(i) = b(j)
      b(j) = k
      j = j + 1

    else

      i = i - 1

    end if

  end do

  return
end
subroutine walker_build_test ( )

!*****************************************************************************80
!
!! WALKER_BUILD_TEST tests WALKER_BUILD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: a(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'WALKER_BUILD_TEST'
  write ( *, '(a)' ) '  WALKER_BUILD builds the Walker sampler data vectors Y'
  write ( *, '(a)' ) '  and A, given a probability vector X.'

  n = 5
  allocate ( x(0:n+1) )

  do i = 1, n
    x(i) = real ( i4_choose ( n - 1, i - 1 ), kind = 8 ) &
         / real ( 2 ** ( n - 1 ), kind = 8 )
  end do

  call r8vec_print ( n + 2, x, &
    '  Binomial PDF (ignore first and last entries):' )

  allocate ( y(0:n+1) )
  allocate ( a(0:n+1) )

  call walker_build ( n, x, y, a )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I    A[I]    Y[i] (ignore first and last entries)'
  write ( *, '(a)' ) ''
  do i = 0, n + 1
    write ( *, '(2x,i2,2x,i2,2x,g14.6)' ) i, a(i), y(i)
  end do
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'WALKER_BUILD_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'

  return
end
subroutine walker_sampler ( n, y, a, seed, i )

!*****************************************************************************80
!
!! WALKER_SAMPLER returns a random sample i=1..N with prob X(i).
!
!  Discussion:
!
!    Implementation of algorithm for sampling from a discrete
!    probability N-vector X(1), X(2), ..., X(N).  (N>=1.)
!    Runs on O(1) worst case time per sample,
!    and uses one integer and one real ( kind = 8 ) N-element array for storage.
!    Preprocessing consumes O(N) time and temporarily uses one 
!    additional integer array (B(0..N+1)) for bookkeeping. 
!    X(0) and X(N+1) are also used as sentinels in the Build() algorithm.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    Original C version by Warren Smith.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Warren Smith,
!    How to sample from a probability distribution,
!    April 18, 2002.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, indicates the size of X.
!
!    Input, real ( kind = 8 ) Y(0:N+1), the Walker threshold vector.
!
!    Input, integer ( kind = 4 ) A(0:N+1), the Walker index vector.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, integer ( kind = 4 ) I, a sample value between 1 and N,
!    selected according to the probability vector X.
!
  implicit none

  integer ( kind = 4 ) a(0:n+1)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) i 
  integer ( kind = 4 ) i4_uniform_ab
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) y(0:n+1)
! 
!  Let i = random uniform integer from {1,2,...N}  
!
  i = i4_uniform_ab ( 1, n, seed ) 

  r = r8_uniform_01 ( seed )

  if ( y(i) < r ) then
    i = a(i)
  end if

  return
end
subroutine walker_sampler_test ( )

!*****************************************************************************80
!
!! WALKER_SAMPLER_TEST tests WALKER_SAMPLER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    Original C version by Warren Smith.
!    This FORTRAN90 version by John Burkardt.
!
  implicit none

  integer ( kind = 4 ), allocatable :: a(:)
  integer ( kind = 4 ), allocatable :: count(:)
  real ( kind = 8 ) expval
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) p
  real ( kind = 8 ) s
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  seed = 123456789
  n = 10
  p = 2.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'WALKER_SAMPLER_TEST:'
  write ( *, '(a)' ) '  WALKER_SAMPLER creates Walker sample vectors Y and A'
  write ( *, '(a)' ) '  for efficiently sampling a discrete probability vector.'
  write ( *, '(a)' ) '  Test the Walker sampler with a Zipf-type probability.'
!
!  Generate a standard Zipf probability vector for cases 1,...,N,
!  with parameter P.
!
  allocate ( x(0:n+1) )
  call zipf_probability ( n, p, x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Zipf probabilities'
  write ( *, '(a,i4)' ) '  for N = ', n
  write ( *, '(a,g14.6)' ) '  and parameter P = ', p
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I     X(I)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, x(i)
  end do
!
!  For better testing, randomly scramble the probabilities.
!
  seed = 123456789
  call random_permutation ( n, x, seed )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Randomly permuted X:'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I     X(I)'
  write ( *, '(a)' ) ''
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6)' ) i, x(i)
  end do
!
!  Build the Walker sampler.
!
  allocate ( y(0:n+1) )
  allocate ( a(0:n+1) )

  call walker_build ( n, x, y, a )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Built the sampler'
  write ( *, '(a)' ) '  i Y(i) A(i):'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6,2x,i4)' ) i, y(i), a(i)
  end do
!
!  Prepare to count the frequency of each outcome.
!
  allocate ( count(0:n+1) )
  count(0:n+1) = 0
!
!  Call the sampler many times.
!
  do i = 1, 100000
    call walker_sampler ( n, y, a, seed, j )
    count(j) = count(j) + 1
  end do
!
!  Compare normalized sample frequencies to the original probabilities in X.
!
  s = 0.0D+00
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  100000 samples:'
  write ( *, '(a)' ) '  prob   #samples:'
  write ( *, '(a)' ) ''

  do i = 1, n
    write ( *, '(2x,g14.6,2x,i6)' ) x(i), count(i)
    expval = x(i) * 100000
    t = expval - count(i)
    s = s + t * t / expval
  end do
  s = s / real ( n, kind = 8 )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6,a)' ) '  sumvar = ', s, ' (should be about 1)'

  return
end
subroutine walker_verify ( n, x, y, a, v )

!*****************************************************************************80
!
!! WALKER_VERIFY verifies a Walker Sampler structure.
!
!  Discussion:
!
!    This test applies the sampling algorithms to a Zipfian distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    Original C version by Warren Smith.
!    This FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, indicates the size of X.
!
!    Input, real ( kind = 8 ) X(0:N+1), contains in X(1) through X(N) the
!    probabilities of outcomes 1 through N.
!
!    Input, real ( kind = 8 ) Y(0:N+1), the Walker threshold vector.
!
!    Input, integer ( kind = 4 ) A(0:N+1), the Walker index vector.
!
!    Output, real ( kind = 8 ) V, the verification sum, which
!    should be close to zero.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(0:n+1)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v
  real ( kind = 8 ) x(0:n+1)
  real ( kind = 8 ) y(0:n+1)
  real ( kind = 8 ) z(0:n+1)
!
!  Reverse the scaling.
!
  z(0:n+1) = y(0:n+1) / real ( n, kind = 8 )
!
!  Add back the adjustments.
!  (Don't try to vectorize this statement!)
!
  do i = 1, n 
    z(a(i)) = z(a(i)) + ( 1.0D+00 - y(i) ) / real ( n, kind = 8 )
  end do
!
!  Check for discrepancies between Z and X.
!
  v = sum ( abs ( z(1:n) - x(1:n) ) )

  return
end
subroutine walker_verify_test ( )

!*****************************************************************************80
!
!! WALKER_VERIFY_TEST tests WALKER_VERIFY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: a(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) v
  real ( kind = 8 ), allocatable :: x(:)
  real ( kind = 8 ), allocatable :: y(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'WALKER_VERIFY_TEST'
  write ( *, '(a)' ) '  WALKER_VERIFY verifies the Walker sampler data'
  write ( *, '(a)' ) '  vectors Y and A,for a given probability vector X.'

  n = 9
  allocate ( x(0:n+1) )

  x(0) = 0.0D+00
  do i = 1, n
    x(i) = log ( 1.0D+00 + 1.0D+00 / real ( i, kind = 8 ) ) &
      / log ( real ( n + 1, kind = 8 ) )
  end do
  x(n+1) = 0.0D+00

  call r8vec_print ( n + 2, x, &
    '  Benford PDF (ignore first and last entries):' )

  allocate ( y(0:n+1) )
  allocate ( a(0:n+1) )

  call walker_build ( n, x, y, a )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   I    A(I)    Y(i) (ignore first and last entries)'
  write ( *, '(a)' ) ''
  do i = 0, n + 1
    write ( *, '(2x,i2,2x,i2,g14.6)' ) i, a(i), y(i)
  end do

  call walker_verify ( n, x, y, a, v )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  The verification sum = ', v
  write ( *, '(a)' ) '  It should be very close to zero.'
!
!  Free memory.
!
  deallocate ( a )
  deallocate ( x )
  deallocate ( y )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'WALKER_VERIFY_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'

  return
end
subroutine zipf_probability ( n, p, x )

!*****************************************************************************80
!
!! ZIPF_PROBABILITY sets up a Zipf probability vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    Original C version by Warren Smith.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    George Zipf,
!    The Psychobiology of Language,
!    1935.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, indicates the size of X.
!
!    Input, real ( kind = 8 ) P, the Zipf parameter.
!    1.0 < P.
!
!    Output, real ( kind = 8 ) X(0:N+1), contains in X(1) through X(N) the
!    probabilities of outcomes 1 through N.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) p
  real ( kind = 8 ) x(0:n+1)

  x(0) = 0.0D+00
  do i = 1, n
    x(i) = real ( i, kind = 8 ) ** ( - p )
  end do
  x(n+1) = 0.0D+00

  call normalize ( n, x )

  return
end
subroutine zipf_probability_test ( )

!*****************************************************************************80
!
!! ZIPF_PROBABILITY_TEST tests ZIPF_PROBABILITY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) p
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ZIPF_PROBABILITY_TEST'
  write ( *, '(a)' ) '  ZIPF_PROBABILITY sets up a probablity vector X of N+2'
  write ( *, '(a)' ) '  elements containing in X[1:N] the probabilities of'
  write ( *, '(a)' ) '  outcomes 1 through Nin a Zipf distribution with'
  write ( *, '(a)' ) '  parameter P.'

  n = 5
  p = 1.0D+00
  allocate ( x(0:n+1) )
  call zipf_probability ( n, p, x )
  call r8vec_print ( n + 2, x, '  X for N = 5, P = 1.0' )
  deallocate ( x )

  n = 5
  p = 2.0D+00
  allocate ( x(0:n+1) )
  call zipf_probability ( n, p, x )
  call r8vec_print ( n + 2, x, '  X for N = 5, P = 2.0' )
  deallocate ( x )

  n = 10
  p = 2.0D+00
  allocate ( x(0:n+1) )
  call zipf_probability ( n, p, x )
  call r8vec_print ( n + 2, x, '  X for N = 10, P = 2.0' )
  deallocate ( x )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ZIPF_PROBABILITY_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'

  return
end
