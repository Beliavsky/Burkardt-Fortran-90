subroutine energy_plot ( it_num, e_plot, header )

!*****************************************************************************80
!
!! ENERGY_PLOT plots the energy as a function of the iterations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IT_NUM, the number of iterations to take.
!
!    Input, real ( kind = 8 ) E_PLOT(0:IT_NUM), the energy per iteration.
!
!    Input, character ( len = * ) HEADER, an identifying string.
!
  implicit none

  integer ( kind = 4 ) it_num

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = 8 ) e_plot(0:it_num)
  character ( len = * ) header
  integer ( kind = 4 ) it
  character ( len = 255 ) plot_filename
!
!  Write data file.
!
  call get_unit ( data_unit )
  data_filename = trim ( header ) // '_energy_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do it = 0, it_num
    if ( 0.0D+00 < e_plot(it) ) then
      write ( data_unit, '(i6,2x,g14.6)' ) it, log ( e_plot(it) )
    end if
  end do
  close ( unit = data_unit )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '  Gnuplot data written to file "' // trim ( data_filename ) // '".'
!
!  Write command file.
!
  call get_unit ( command_unit )
  command_filename = trim ( header ) // '_energy_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  plot_filename = trim ( header ) // '_energy.png'

  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set xlabel "<---Iteration--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---Log(Energy)--->"'
  write ( command_unit, '(a)' ) 'set title "Energy Decrease with Iteration"'
  write ( command_unit, '(a,i4,a)' ) 'plot "' // trim ( data_filename ) &
    // '" using 1:2 with points pt 7 ps 1'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Gnuplot commands written to "' &
    // trim ( command_filename ) // '".'

  return
end
subroutine evolution_plot ( n, it_num, x_plot, header )

!*****************************************************************************80
!
!! EVOLUTION_PLOT plots all points as a function of the iterations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, integer ( kind = 4 ) IT_NUM, the number of iterations to take.
!
!    Input, real ( kind = 8 ) X_PLOT(N,IT_NUM), the point locations over time.
!
!    Input, character ( len = * ) HEADER, an identifying string.
!
  implicit none

  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) n

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = * ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it
  character ( len = 255 ) plot_filename
  real ( kind = 8 ) x_plot(n,0:it_num)
!
!  Write data file.
!
  call get_unit ( data_unit )
  data_filename = trim ( header ) // '_evolution_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )

  do it = 0, it_num
    write ( data_unit, '(i6)', advance = 'no' ) it
    do i = 1, n
      write ( data_unit, '(g14.6)', advance = 'no' ) x_plot(i,it)
    end do
    write ( data_unit, '(a)', advance = 'yes' )
  end do

  close ( unit = data_unit )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '  Gnuplot data written to file "' // trim ( data_filename ) // '".'
!
!  Write command file.
!
  call get_unit ( command_unit )
  command_filename = trim ( header ) // '_evolution_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  plot_filename = trim ( header ) // '_evolution.png'

  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---Iteration--->"'
  write ( command_unit, '(a)' ) 'set title "Point Motion with Iteration"'
  write ( command_unit, '(a,i4,a)' ) 'plot for [i=2:', &
    n + 1, &
    '] "' // trim ( data_filename ) // '" using i:1 with points pt 7 ps 1'

  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Gnuplot commands written to "' &
    // trim ( command_filename ) // '".'

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
subroutine line_ccvt_lloyd ( n, a, b, it_num, header, x )

!*****************************************************************************80
!
!! LINE_CCVT_LLOYD carries out the constrained Lloyd algorithm.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of generators.
!
!    Input, real ( kind = 8 ) A, B, the left and right endpoints.
!
!    Input, integer ( kind = 4 ) IT_NUM, the number of iterations to take.
!
!    Input, character ( len = * ) HEADER, an identifying string.
!
!    Input/output, real ( kind = 8 ) X(N), the point locations.
!
  implicit none

  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) e
  real ( kind = 8 ) e_plot(0:it_num)
  character ( len = * ) header
  integer ( kind = 4 ) it
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_old(n)
  real ( kind = 8 ) x_plot(n,0:it_num)
  real ( kind = 8 ) xm
  real ( kind = 8 ) xm_plot(1:it_num)

  call line_cvt_energy ( n, a, b, x, e )
  e_plot(0) = e
  x_plot(1:n,0) = x(1:n)

  do it = 1, it_num

    x_old(1:n) = x(1:n)

    call line_ccvt_lloyd_step ( n, a, b, x )

    x_plot(1:n,it) = x(1:n)

    call line_cvt_energy ( n, a, b, x, e )
    e_plot(it) = e

    xm = sum ( ( x_old(1:n) - x(1:n) ) ** 2 ) / real ( n, kind = 8 )
    xm_plot(it) = xm
    
  end do

  call energy_plot ( it_num, e_plot, header )
  call motion_plot ( it_num, xm_plot, header )
  call evolution_plot ( n, it_num, x_plot, header )

  return
end
subroutine line_ccvt_lloyd_step ( n, a, b, x )

!*****************************************************************************80
!
!! LINE_CCVT_LLOYD_STEP takes one step of Lloyd's constrained CVT algorithm.
!
!  Discussion:
!
!    Each step of Lloyd's algorithm replaces a point by the center of mass
!    of the associated region.  For points on a line, with a uniform
!    density, the associated region is demarcated by the midways between 
!    successive points.
!
!    Here, we include the additional constraint that we want the first and last
!    points to be fixed at the endpoints of the line, that is, X(1) = A
!    and X(2) = B.  In that case, the calculation of the updates for the
!    first two and last two points must be handled differently.
!
!    For points away from the boundary, a step of Lloyd's method can be 
!    regarded as replacing each point by the average of the left and right
!    midways.  The midways, of course, are the average of two points.
!    So for point J, we have:
!
!      M(J-1,J) = ( X(J-1) + X(J) ) / 2
!      M(J,J+1) = ( X(J) + X(J+1) ) / 2
!      X*(J) = ( M(J-1,J) + M(J,J+1) ) / 2 = ( X(J-1) + 2 X(J) + X(J+1) ) / 4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!    1 <= N.
!
!    Input, real ( kind = 8 ) A, B, the left and right endpoints.
!
!    Input/output, real ( kind = 8 ) X(N), the point locations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_old(n)

  x_old(1:n) = x(1:n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else if ( n == 2 ) then

    x(1) = a
    x(2) = b

  else

    x(1) = a

    do j = 2, n - 1 
      x(j) = ( 0.5D+00 * ( x_old(j-1) + x_old(j) ) &
             + 0.5D+00 * ( x_old(j) + x_old(j+1) ) ) / 2.0D+00
    end do

    x(n) = b

  end if

  return
end
subroutine line_cvt_energy ( n, a, b, x, e )

!*****************************************************************************80
!
!! LINE_CVT_ENERGY computes the CVT energy for a given set of generators.
!
!  Discussion:
!
!    Given a set of generators G over the line [A,B], then the energy
!    is defined as
!      E = integral ( a <= x <= b ) ( x - g(x) )^2 dx
!    where g(x) is the nearest generator to the point x.
!
!    For the 1D case, this integral can be evaluated exactly as the
!    sum of integrals over each subinterval:
!
!      E(i) = integral ( xl <= x <= xr ) ( x - x(i) )^2 dx
!           = ( ( x(i) - xl )^3 + ( xr - x(i) )^3 ) / 3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of generators.
!
!    Input, real ( kind = 8 ) A, B, the left and right endpoints.
!
!    Input, real ( kind = 8 ) X(N), the generator locations.
!
!    Output, real ( kind = 8 ) E, the energy of the generator distribution.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) e
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xl
  real ( kind = 8 ) xr

  e = 0.0D+00

  do j = 1, n

    if ( j == 1 ) then
      xl = a
    else
      xl = ( x(j-1) + x(j) ) / 2.0D+00
    end if

    if ( j == n ) then
      xr = b
    else
      xr = ( x(j) + x(j+1) ) / 2.0D+00
    end if

    e = e + ( ( x(j) - xl ) ** 3 + ( xr - x(j) ) ** 3  ) / 3.0D+00

  end do

  return
end
subroutine line_cvt_lloyd ( n, a, b, it_num, header, x )

!*****************************************************************************80
!
!! LINE_CVT_LLOYD carries out the Lloyd algorithm.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of generators.
!
!    Input, real ( kind = 8 ) A, B, the left and right endpoints.
!
!    Input, integer ( kind = 4 ) IT_NUM, the number of iterations to take.
!
!    Input, character ( len = * ) HEADER, an identifying string.
!
!    Input/output, real ( kind = 8 ) X(N), the point locations.
!
  implicit none

  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) e
  real ( kind = 8 ) e_plot(0:it_num)
  character ( len = * ) header
  integer ( kind = 4 ) it
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_old(n)
  real ( kind = 8 ) x_plot(n,0:it_num)
  real ( kind = 8 ) xm
  real ( kind = 8 ) xm_plot(1:it_num)

  call line_cvt_energy ( n, a, b, x, e )

  e_plot(0) = e
  x_plot(1:n,0) = x(1:n)

  do it = 1, it_num

    x_old(1:n) = x(1:n)

    call line_cvt_lloyd_step ( n, a, b, x )

    x_plot(1:n,it) = x(1:n)

    call line_cvt_energy ( n, a, b, x, e )
    e_plot(it) = e

    xm = sum ( ( x_old(1:n) - x(1:n) ) ** 2 ) / real ( n, kind = 8 )
    xm_plot(it) = xm
    
  end do

  call energy_plot ( it_num, e_plot, header )
  call motion_plot ( it_num, xm_plot, header )
  call evolution_plot ( n, it_num, x_plot, header )

  return
end
subroutine line_cvt_lloyd_step ( n, a, b, x )

!*****************************************************************************80
!
!! LINE_CVT_LLOYD_STEP takes one step of Lloyd's unconstrained CVT algorithm.
!
!  Discussion:
!
!    Each step of Lloyd's algorithm replaces a point by the center of mass
!    of the associated region.  For points on a line, with a uniform
!    density, the associated region is demarcated by the midways between 
!    successive points.
!
!    For points away from the boundary, a step of Lloyd's method can be 
!    regarded as replacing each point by the average of the left and right
!    midways.  The midways, of course, are the average of two points.
!    So for point J, we have:
!
!      M(J-1,J) = ( X(J-1) + X(J) ) / 2
!      M(J,J+1) = ( X(J) + X(J+1) ) / 2
!      X*(J) = ( M(J-1,J) + M(J,J+1) ) / 2 = ( X(J-1) + 2 X(J) + X(J+1) ) / 4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of points.
!    1 <= N.
!
!    Input, real ( kind = 8 ) A, B, the left and right endpoints.
!
!    Input, real ( kind = 8 ) X(N), the point locations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_old(n)

  x_old(1:n) = x(1:n)

  if ( n == 1 ) then

    x(1) = ( a + b ) / 2.0D+00

  else

    j = 1
    x(j) = (           a                     &
             + 0.5D+00 * ( x_old(j) + x_old(j+1) ) ) / 2.0D+00

    do j = 2, n - 1 
      x(j) = ( 0.5D+00 * ( x_old(j-1) + x_old(j) ) &
             + 0.5D+00 * ( x_old(j) + x_old(j+1) ) ) / 2.0D+00
    end do

    j = n
    x(j) =   ( 0.5D+00 * ( x_old(j-1) + x_old(j) ) &
             +                                 b )   / 2.0D+00

  end if

  return
end
subroutine motion_plot ( it_num, xm_plot, header )

!*****************************************************************************80
!
!! MOTION_PLOT plots the motion as a function of the iterations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IT_NUM, the number of iterations to take.
!
!    Input, real ( kind = 8 ) XM_PLOT(IT_NUM), the average motion per iteration.
!
!    Input, character ( len = * ) HEADER, an identifying string.
!
  implicit none

  integer ( kind = 4 ) it_num

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = * ) header
  integer ( kind = 4 ) it
  character ( len = 255 ) plot_filename
  real ( kind = 8 ) xm_plot(0:it_num)
!
!  Write data file.
!
  call get_unit ( data_unit )
  data_filename = trim ( header ) // '_motion_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do it = 1, it_num
    if ( 0.0D+00 < xm_plot(it) ) then
      write ( data_unit, '(i6,2x,g14.6)' ) it, log ( xm_plot(it) )
    end if
  end do
  close ( unit = data_unit )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '  Gnuplot data written to file "' // trim ( data_filename ) // '".'
!
!  Write command file.
!
  call get_unit ( command_unit )
  command_filename = trim ( header ) // '_motion_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  plot_filename = trim ( header ) // '_motion.png'

  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set xlabel "<---Iteration--->"'
  write ( command_unit, '(a)' ) 'set ylabel "<---Average Motion--->"'
  write ( command_unit, '(a)' ) 'set title "Generator Motion with Iteration"'
  write ( command_unit, '(a,i4,a)' ) 'plot "' // trim ( data_filename ) &
    // '" using 1:2 with points pt 7 ps 1'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Gnuplot commands written to "' &
    // trim ( command_filename ) // '".'

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
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items in the vector.
!    N must be positive.
!
!    Input/output, real ( kind = 8 ) A(N).
!    On input, the array to be sorted;
!    On output, the array has been sorted.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which 
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
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
