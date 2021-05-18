subroutine cvtp_find_closest ( m, n, x, generator, width, modular, nearest )

!*****************************************************************************80
!
!! CVTP_FIND_CLOSEST finds the Voronoi cell generator closest to a point X.
!
!  Discussion:
!
!    This routine finds the closest Voronoi cell generator by checking every
!    one.  For problems with many cells, this process can take the bulk
!    of the CPU time.  Other approaches, which group the cell generators into
!    bins, can run faster by a large factor.
!
!    For this routine, if MODULAR is TRUE, then distance is done in a modular 
!    sense, as though the points were on a generalized torus.  It's simple, 
!    really, we just need, in each coordinate, to consider 
!
!     X(I)-WIDTH(I), X(I), and X(I)+WIDTH(I).
!
!    The bad part is, to keep our sanity, we want to replace X on output
!    by the actual coordinates that got closest to some generator G,
!    even though some of these coordinates may lie outside the unit
!    hypercube.  This is the right thing to do, so that the averaging
!    process works correctly.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    25 July 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of cell generators.
!
!    Input, real ( kind = 8 ) X(M), the point to be checked.
!
!    Input, real ( kind = 8 ) GENERATOR(M,N), the cell generators.
!
!    Input, real ( kind = 8 ) WIDTH(M), the width of the region in 
!    each dimension.
!
!    Input, logical MODULAR, is TRUE if modular arithmetic is to be used.
!
!    Output, integer ( kind = 4 ) NEAREST, the index of the nearest cell
!    generators.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) generator(m,n)
  real ( kind = 8 ) dist_sq_min
  real ( kind = 8 ) dist_sq
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  logical modular
  integer ( kind = 4 ) nearest
  real ( kind = 8 ) side
  real ( kind = 8 ) side1
  real ( kind = 8 ) side2
  real ( kind = 8 ) side3
  real ( kind = 8 ) width(m)
  real ( kind = 8 ) x(m)
  real ( kind = 8 ) y(m)
  real ( kind = 8 ) z(m)

  nearest = 0
  dist_sq_min = huge ( dist_sq_min )

  do i = 1, n

    dist_sq = 0.0D+00

    do j = 1, m

      if ( modular ) then

        side1 = abs ( generator(j,i)            - x(j) )
        side2 = abs ( generator(j,i) + width(j) - x(j) )
        side3 = abs ( generator(j,i) - width(j) - x(j) )
      
        if ( side2 < side1 .and. side2 < side3 ) then
          side = side2
          y(j) = x(j) - width(j)
        else if ( side3 < side1 .and. side3 < side2 ) then
          side = side3
          y(j) = x(j) + width(j)
        else
          side = side1
          y(j) = x(j)
        end if

      else

        side = abs ( generator(j,i)            - x(j) )
        y(j) = x(j)

      end if

      dist_sq = dist_sq + side ** 2

    end do

    if ( dist_sq < dist_sq_min ) then
      dist_sq_min = dist_sq
      nearest = i
      z(1:m) = y(1:m)
    end if

  end do
!
!  Overwrite X by Z, which is equal to X in modular arithmetic,
!  but which is the closest to generator "NEAREST" (in non-modular
!  arithmetic) of all the modularly equivalent copies of X.
!
  x(1:m) = z(1:m)

  return
end
subroutine cvtp_iteration ( m, n, generator, width, modular, &
  sample_num_cvt, change_l2 )

!*****************************************************************************80
!
!! CVTP_ITERATION takes one step of the CVT iteration.
!
!  Discussion:
!
!    The routine is given a set of points, called "generators", which
!    define a tessellation of the region into Voronoi cells.  Each point
!    defines a cell.  Each cell, in turn, has a centroid, but it is
!    unlikely that the centroid and the generator coincide.
!
!    Each time this CVT iteration is carried out, an attempt is made
!    to modify the generators in such a way that they are closer and
!    closer to being the centroids of the Voronoi cells they generate.
!
!    A large number of sample points are generated, and the nearest generator
!    is determined.  A count is kept of how many points were nearest to each
!    generator.  Once the sampling is completed, the location of all the
!    generators is adjusted.  This step should decrease the discrepancy
!    between the generators and the centroids.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 Decemberc 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of Voronoi cells.
!
!    Input/output, real ( kind = 8 ) GENERATOR(M,N), the Voronoi
!    cell generators.  On output, these have been modified
!
!    Input, real ( kind = 8 ) WIDTH(M), the width of the region in 
!    each direction.
!
!    Input, logical MODULAR, is TRUE if modular arithmetic is to be used.
!
!    Input, integer ( kind = 4 ) SAMPLE_NUM_CVT, the number of sample points.
!
!    Output, real ( kind = 8 ) CHANGE_L2, the sum of the L2 norms of the
!    change in each generator's position.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) generator(m,n)
  real ( kind = 8 ) generator2(m,n)
  real ( kind = 8 ) change_gen
  real ( kind = 8 ) change_l2
  integer ( kind = 4 ) count(n)
  logical, parameter :: debug = .false.
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  logical modular
  integer ( kind = 4 ) nearest
  logical reset
  integer ( kind = 4 ) sample_num_cvt
  real ( kind = 8 ) side
  real ( kind = 8 ) side1
  real ( kind = 8 ) side2
  real ( kind = 8 ) side3
  real ( kind = 8 ) width(m)
  real ( kind = 8 ) x(m)

  generator2(1:m,1:n) = 0.0D+00
  count(1:n) = 0
  reset = .true.

  do j = 1, sample_num_cvt
!
!  Generate a sampling point X.
!
    call cvtp_region_sampler ( m, 1, x, width )

    reset = .false.
!
!  Find the nearest cell generator G.
!
!  Note that, to account for the modular arithemetic that is
!  implicitly employed, the input value of X will be altered to
!  the representative of X whose modular value is equal to X,
!  but whose actual value is the closest to the generator G
!  of all representatives of X.
!
!  Otherwise, the averaging mechanism would be invalid!
!
    call cvtp_find_closest ( m, n, x, generator, width, modular, nearest )
!
!  Add X to the averaging data for GENERATOR(*,NEAREST).
!
    generator2(1:m,nearest) = generator2(1:m,nearest) + x(1:m)

    count(nearest) = count(nearest) + 1

  end do
!
!  Compute the new generators.
!
  do j = 1, n
    if ( count(j) /= 0 ) then
      generator2(1:m,j) = generator2(1:m,j) / real ( count(j), kind = 8 )
    end if
  end do
!
!  It's possible that the generator would go outside the box.
!  Use modular arithmetic to fix that.
!
  if ( modular ) then

    do j = 1, n
      do i = 1, m

        if ( generator2(i,j) < 0.0D+00 ) then
          generator2(i,j) = generator2(i,j) + width(i)
        else if ( width(i) < generator2(i,j) ) then
          generator2(i,j) = generator2(i,j) - width(i)
        end if

      end do
    end do

  end if
!
!  Determine the L2 norm of the change in the dataset.
!
!  Because of our modular arithmetic, we need to do this carefully.
!
  change_l2 = 0.0D+00

  do j = 1, n

    change_gen = 0.0D+00

    if ( modular ) then

      do i = 1, m
        side1 = abs ( generator2(i,j)            - generator(i,j) )
        side2 = abs ( generator2(i,j) - width(i) - generator(i,j) )
        side3 = abs ( generator2(i,j) + width(i) - generator(i,j) )
        side = min ( side1, side2, side3 )
        change_gen = change_gen + side ** 2
      end do

    else

      do i = 1, m
        side = abs ( generator2(i,j)            - generator(i,j) )
        change_gen = change_gen + side ** 2
      end do

    end if

    change_l2 = change_l2 + sqrt ( change_gen )

  end do
!
!  Update.
!
  generator(1:m,1:n) = generator2(1:m,1:n)

  return
end
subroutine cvtp_region_sampler ( m, n, x, width )

!*****************************************************************************80
!
!! CVTP_REGION_SAMPLER returns a sample point in the physical region.
!
!  Discussion:
!
!    This code differs from the original CVT code only in that
!    the WIDTH variable is available to specify the width of
!    the box in each coordinate direction.  Originally, the unit
!    hypercube was used, and in fact, the current version of this
!    program isn't really able to change the default widths from 1,
!    but at least now, formally, the machinery is in place.
!
!    This routine original interfaced with a lower routine called
!    TEST_REGION, which tested whether the points generated in the
!    bounding box were actually inside a possibly smaller physical
!    region of interest.  It's been a long time since that option
!    was actually used, so it's been dropped.
!
!    A point is chosen in the bounding box, either by a uniform random
!    number generator, or from a vector Halton sequence.
!
!    The original coding for this routine only supported a Halton
!    sequence of dimension 3 or less.  This restriction has been removed.
!
!    Note that RESET was made an input-only quantity, in part to match
!    the behavior of the routine in MATLAB, where it's cumbersome to
!    support an input/output variable.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    05 December 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of points to generate now.
!
!    Output, real ( kind = 8 ) X(M,N), the sample points.
!
!    Input, real ( kind = 8 ) WIDTH(M), the width of the region 
!    in each dimension.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
 
  integer ( kind = 4 ) i
  real ( kind = 8 ) width(m)
  real ( kind = 8 ) x(m,n)

  call random_number ( harvest = x(1:m,1:n) )
!
!  Stretch the points to the given widths.
!
  do i = 1, m
    x(i,1:n) = width(i) * x(i,1:n)
  end do

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 September 2005
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
function r8_uniform_01 ( seed )

!*****************************************************************************80
!
!! R8_UNIFORM_01 returns a unit pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer ( kind = 4 ) variable.
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
    seed = seed + 2147483647
  end if
!
!  Although SEED can be represented exactly as a 32 bit integer,
!  it generally cannot be represented exactly as a 32 bit real number!
!
  r8_uniform_01 = real ( seed, kind = 8 ) * 4.656612875D-10

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
!    Input, real ( kind = 8 ) TABLE(M,N), the table data.
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
    stop
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

