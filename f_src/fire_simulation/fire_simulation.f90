program main

!*****************************************************************************80
!
!! MAIN is the main program for fire_simulation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2013.
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: forest_size = 20

  integer ( kind = 4 ) forest(forest_size,forest_size)
  logical forest_is_burning
  integer ( kind = 4 ) i_ignite
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j_ignite
  real ( kind = 8 ) percent
  real ( kind = 8 ), parameter :: prob_spread = 0.5D+00
  integer ( kind = 4 ) seed

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'fire_simulation'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  A probabilistic simulation of a forest fire.'
  write ( *, '(a,g14.6)' ) '  The probability of tree-to-tree spread is ', prob_spread
!
!  Initialize the random number generator.
!
  call get_seed ( seed )
  write ( *, '(a,i12)' ) '  The random number generator is seeded by ', seed
!
!  Initialize the values in the forest.
!
  call forest_initialize ( forest_size, forest )
!
!  Choose a tree at random where the fire will start.
!
  i_ignite = i4_uniform_ab ( 1, forest_size, seed )
  j_ignite = i4_uniform_ab ( 1, forest_size, seed )
  call tree_ignite ( forest_size, forest, i_ignite, j_ignite )
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2,a)' ) '  Fire starts at tree(', i_ignite, ',', j_ignite, ')'
!
!  Let time run until nothing is burning any more.
!
  do while ( forest_is_burning ( forest_size, forest ) )
    call forest_burns ( forest_size, forest, seed, prob_spread )
  end do
!
!  Display the final forest state.
!
  call forest_print ( forest_size, forest, i_ignite, j_ignite )
!
!  Report the percentage of forest burned.
!
  call get_percent_burned ( forest_size, forest, percent )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Percentage of forest burned = ', percent
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'fire_simulation:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop
end
function fire_spreads ( seed, prob_spread ) 

!*****************************************************************************80
!
!! FIRE_SPREADS determines whether the fire spreads.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Input, real ( kind = 8 ) PROB_SPREAD, the probability of spreading.
!
!    Output, logical FIRE_SPREADS, is TRUE if the fire spreads.
!
  implicit none

  real ( kind = 8 ) prob_spread
  logical fire_spreads
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u

  u = r8_uniform_01 ( seed )

  if ( u < prob_spread ) then
    fire_spreads = .true.
  else
    fire_spreads = .false.
  end if
 
  return
end
subroutine forest_burns ( forest_size, forest, seed, prob_spread )

!*****************************************************************************80
!
!! FOREST_BURNS models a single time step of the burning forest.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) FOREST_SIZE, the linear dimension of the forest.
!
!    Input/output, integer ( kind = 4 ) FOREST(FOREST_SIZE,FOREST_SIZE), an
!    array with an entry for each tree in the forest.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Input, real ( kind = 8 ) PROB_SPREAD, the probability that the fire will 
!    spread from a burning tree to an unburnt one.
!
  implicit none

  integer ( kind = 4 ) forest_size

  integer ( kind = 4 ), parameter :: BURNING = 2
  integer ( kind = 4 ), parameter :: BURNT = 3
  logical fire_spreads
  integer ( kind = 4 ) forest(forest_size,forest_size)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) prob_spread
  integer ( kind = 4 ) seed
  integer ( kind = 4 ), parameter :: SMOLDERING = 1
  integer ( kind = 4 ), parameter :: UNBURNT = 0
!
!  Burning trees burn down;
!  Smoldering trees ignite;
!
  do j = 1, forest_size
    do i = 1, forest_size
      if ( forest(i,j) == BURNING ) then
        forest(i,j) = BURNT
      else if ( forest(i,j) == SMOLDERING ) then
        forest(i,j) = BURNING
      end if
    end do
  end do
!
!  Unburnt trees might catch fire.
!
  do j = 1, forest_size
    do i = 1, forest_size

      if ( forest(i,j) == BURNING ) then
!
!  North.
!
        if ( 1 < i ) then
          if ( fire_spreads ( seed, prob_spread ) .and. forest(i-1,j) == UNBURNT ) then
            forest(i-1,j) = SMOLDERING
          end if
        end if
!
!  South.
!
        if ( i < forest_size ) then
          if ( fire_spreads ( seed, prob_spread ) .and. forest(i+1,j) == UNBURNT ) then
            forest(i+1,j) = SMOLDERING
          end if
        end if
!
!  West.
!
        if ( 1 < j ) then
          if ( fire_spreads ( seed, prob_spread ) .and. forest(i,j-1) == UNBURNT ) then
            forest(i,j-1) = SMOLDERING
          end if
        end if
!
!  East.
!
        if ( j < forest_size ) then
          if ( fire_spreads ( seed, prob_spread ) .and. forest(i,j+1) == UNBURNT ) then
            forest(i,j+1) = SMOLDERING
          end if
        end if

      end if

    end do
  end do

  return
end
subroutine forest_initialize ( forest_size, forest ) 

!*****************************************************************************80
!
!! FOREST_INITIALIZE initializes the forest values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) FOREST_SIZE, the linear dimension of the forest.
!
!    Output, integer ( kind = 4 ) FOREST(FOREST_SIZE,FOREST_SIZE), an array
!    with an entry for each tree in the forest.
!
  implicit none

  integer ( kind = 4 ) forest_size

  integer ( kind = 4 ) forest(forest_size,forest_size)
  integer ( kind = 4 ), parameter :: UNBURNT = 0

  forest(1:forest_size,1:forest_size) = UNBURNT

  return
end
function forest_is_burning ( forest_size, forest ) 

!*****************************************************************************80
!
!! FOREST_IS_BURNING reports whether any trees in the forest are burning.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) FOREST_SIZE, the linear dimension of the forest.
!
!    Input, integer ( kind = 4 ) FOREST(FOREST_SIZE,FOREST_SIZE), an array
!    with an entry for each tree in the forest.
!
!    Output, logical FOREST_IS_BURNING, is TRUE if any tree in the forest
!    is in the SMOLDERING or BURNING state.
!
  implicit none

  integer ( kind = 4 ) forest_size

  integer ( kind = 4 ), parameter :: BURNING = 2
  integer ( kind = 4 ) forest(forest_size,forest_size)
  logical forest_is_burning
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ), parameter :: SMOLDERING = 1
  logical value

  value = .false.

  do j = 1, forest_size
    do i = 1, forest_size
      if ( forest(i,j) == SMOLDERING .or. forest(i,j) == BURNING ) then
        value = .true.
      end if
    end do
  end do

  forest_is_burning = value

  return
end
subroutine forest_print ( forest_size, forest, i_ignite, j_ignite )

!*****************************************************************************80
!
!! FOREST_PRINT prints the state of the trees in the forest.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) FOREST_SIZE, the linear dimension of the forest.
!
!    Input, integer ( kind = 4 ) FOREST(FOREST_SIZE,FOREST_SIZE), an array
!    with an entry for each tree in the forest.
!
!    Input, integer ( kind = 4 ) I_IGNITE, J_IGNITE, the location of the start 
!    of the fire.
!
  implicit none

  integer ( kind = 4 ) forest_size

  integer ( kind = 4 ), parameter :: BURNT = 3
  integer ( kind = 4 ) forest(forest_size,forest_size)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_ignite
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j_ignite

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Map of fire damage.'
  write ( *, '(a)' ) '  Fire started at "*".'
  write ( *, '(a)' ) '  Burned trees are indicated by ".".'
  write ( *, '(a)' ) '  Unburned trees are indicated by "X".'
  write ( *, '(a)' ) ''

  do i = 1, forest_size
    write ( *, '(a)', ADVANCE='NO' ) '  '
    do j = 1, forest_size
      if ( i == i_ignite .and. j == j_ignite ) then
        write ( *, '(a)', ADVANCE='NO' ) '*'
      else if ( forest(i,j) == BURNT ) then
        write ( *, '(a)', ADVANCE='NO' ) '.'
      else
        write ( *, '(a)', ADVANCE='NO' ) 'X'
      end if
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine get_percent_burned ( forest_size, forest, percent ) 

!*****************************************************************************80
!
!! GET_PERCENT_BURNED computes the percentage of the forest that burned.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) FOREST_SIZE, the linear dimension of the forest.
!
!   Input, integer ( kind = 4 ) FOREST(FOREST_SIZE,FOREST_SIZE), an array
!    with an entry for each tree in the forest.
!
!    Output, real ( kind = 8 ) PERCENT, the percentage of the forest
!    that burned.
!
  implicit none

  integer ( kind = 4 ) forest_size

  integer ( kind = 4 ), parameter :: BURNT = 3
  integer ( kind = 4 ) forest(forest_size,forest_size)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) percent
  integer ( kind = 4 ) total

  total = 0
  do j = 1, forest_size
    do i = 1, forest_size
      if ( forest(i,j) == BURNT ) then
        total = total + 1
      end if
    end do
  end do

  percent = real ( total, kind = 8 ) &
    / real ( forest_size * forest_size, kind = 8 )

  return
end
subroutine get_seed ( seed )

!*****************************************************************************80
!
!! GET_SEED returns a seed for the random number generator.
!
!  Discussion:
!
!    The seed depends on the current time, and ought to be (slightly)
!    different every millisecond.  Once the seed is obtained, a random
!    number generator should be called a few times to further process
!    the seed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) SEED, a pseudorandom seed value.
!
  implicit none

  integer ( kind = 4 ), parameter :: i4_huge = 2147483647
  integer ( kind = 4 ) seed
  real ( kind = 8 ) temp
  character ( len = 10 ) time
  character ( len = 8 ) today
  integer ( kind = 4 ) values(8)
  character ( len = 5 ) zone

  call date_and_time ( today, time, zone, values )

  temp = 0.0D+00

  temp = temp + real ( values(2) - 1, kind = 8 ) /  11.0D+00
  temp = temp + real ( values(3) - 1, kind = 8 ) /  30.0D+00
  temp = temp + real ( values(5),     kind = 8 ) /  23.0D+00
  temp = temp + real ( values(6),     kind = 8 ) /  59.0D+00
  temp = temp + real ( values(7),     kind = 8 ) /  59.0D+00
  temp = temp + real ( values(8),     kind = 8 ) / 999.0D+00
  temp = temp                                    /   6.0D+00

  do while ( temp <= 0.0D+00 )
    temp = temp + 1.0D+00
  end do

  do while ( 1.0D+00 < temp )
    temp = temp - 1.0D+00
  end do

  seed = int ( real ( i4_huge, kind = 8 ) * temp )
!
!  Never use a seed of 0 or maximum integer.
!
  if ( seed == 0 ) then
    seed = 1
  end if

  if ( seed == i4_huge ) then
    seed = seed - 1
  end if

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
    stop
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
    stop
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + i4_huge
  end if

  r8_uniform_01 = real ( seed, kind = 8 ) * 4.656612875D-10

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
subroutine tree_ignite ( forest_size, forest, i_ignite, j_ignite )

!*****************************************************************************80
!
!! TREE_IGNITE sets a given tree to the SMOLDERING state.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2013
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) FOREST_SIZE, the linear dimension of 
!    the forest.
!
!    Input, integer ( kind = 4 ) FOREST(FOREST_SIZE,FOREST_SIZE), an array
!    with an entry for each tree in the forest.
!
!    Input, integer ( kind = 4 ) I_IGNITE, J_IGNITE, the coordinates of the 
!    tree which is to be set to SMOLDERING.
!
  implicit none

  integer ( kind = 4 ) forest_size

  integer ( kind = 4 ) forest(forest_size,forest_size)
  integer ( kind = 4 ) i_ignite
  integer ( kind = 4 ) j_ignite
  integer ( kind = 4 ), parameter :: SMOLDERING = 1

  forest(i_ignite,j_ignite) = SMOLDERING

  return
end
