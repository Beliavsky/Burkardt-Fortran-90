subroutine all_gms ( nu, rho, n, x, y, t, &
  u, dudt, dudx, dudxx, dudy, dudyy, us, &
  v, dvdt, dvdx, dvdxx, dvdy, dvdyy, vs, &
  p, dpdt, dpdx, dpdxx, dpdy, dpdyy, ps )

!*****************************************************************************80
!
!! all_gms evaluates the variables of the GMS flow.
!
!  Discussion:
!
!    The flow has been modified by a sign change that makes it slightly
!    more plausible physically.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the fluid density.
!
!    integer( kind = 4 ) N, the number of nodes.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of nodes.
!
!    real ( kind = 8 ) T, the current time.
!
!  Output:
!
!    real ( kind = 8 ) U(N), DUDT(N), DUDX(N), DUDXX(N), DUDY(N), DUDYY(N), US(N),
!    the horizontal velocity values.
!
!    real ( kind = 8 ) V(N), DVDT(N), DVDX(N), DVDXX(N), DVDY(N), DVDYY(N), VS(N),
!    the vertical velocity values.
!
!    real ( kind = 8 ) P(N), DPDT(N), DPDX(N), DPDXX(N), DPDY(N), DPDYY(N), PS(N),
!    the pressure values.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) u(n)
  real ( kind = 8 ) dudt(n)
  real ( kind = 8 ) dudx(n)
  real ( kind = 8 ) dudxx(n)
  real ( kind = 8 ) dudy(n)
  real ( kind = 8 ) dudyy(n)
  real ( kind = 8 ) us(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) dvdt(n)
  real ( kind = 8 ) dvdx(n)
  real ( kind = 8 ) dvdxx(n)
  real ( kind = 8 ) dvdy(n)
  real ( kind = 8 ) dvdyy(n)
  real ( kind = 8 ) vs(n)
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) dpdt(n)
  real ( kind = 8 ) dpdx(n)
  real ( kind = 8 ) dpdxx(n)
  real ( kind = 8 ) dpdy(n)
  real ( kind = 8 ) dpdyy(n)
  real ( kind = 8 ) ps(n)
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) nu
  real ( kind = 8 ) rho
  real ( kind = 8 ) s(n)
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  s = ( -1.0D+00 ) ** ( ceiling ( x ) + ceiling ( y ) )

  u =             pi    * s * sin ( t ) * ( sin (       pi * x ) )**2  * sin ( 2.0 * pi * y )
  dudt =          pi    * s * cos ( t ) * ( sin (       pi * x ) )**2  * sin ( 2.0 * pi * y )
  dudx =          pi**2 * s * sin ( t ) *   sin ( 2.0 * pi * x )       * sin ( 2.0 * pi * y )
  dudxx =   2.0 * pi**3 * s * sin ( t ) *   cos ( 2.0 * pi * x )       * sin ( 2.0 * pi * y )
  dudy =    2.0 * pi**2 * s * sin ( t ) * ( sin (       pi * x ) )**2  * cos ( 2.0 * pi * y )
  dudyy = - 4.0 * pi**3 * s * sin ( t ) * ( sin (       pi * x ) )**2  * sin ( 2.0 * pi * y )

  v =           - pi    * s * sin ( t ) * sin ( 2.0 * pi * x ) * ( sin (       pi * y ) )**2
  dvdt =        - pi    * s * cos ( t ) * sin ( 2.0 * pi * x ) * ( sin (       pi * y ) )**2
  dvdx =  - 2.0 * pi**2 * s * sin ( t ) * cos ( 2.0 * pi * x ) * ( sin (       pi * y ) )**2
  dvdxx =   4.0 * pi**3 * s * sin ( t ) * sin ( 2.0 * pi * x ) * ( sin (       pi * y ) )**2
  dvdy =        - pi**2 * s * sin ( t ) * sin ( 2.0 * pi * x ) *   sin ( 2.0 * pi * y )
  dvdyy = - 2.0 * pi**3 * s * sin ( t ) * sin ( 2.0 * pi * x ) *   cos ( 2.0 * pi * y )

  p =               rho * s * sin ( t ) * cos ( pi * x ) * sin ( pi * y )
  dpdt =            rho * s * cos ( t ) * cos ( pi * x ) * sin ( pi * y )
  dpdx =  - pi    * rho * s * sin ( t ) * sin ( pi * x ) * sin ( pi * y )
  dpdxx = - pi**2 * rho * s * sin ( t ) * cos ( pi * x ) * sin ( pi * y )
  dpdy =    pi    * rho * s * sin ( t ) * cos ( pi * x ) * cos ( pi * y )
  dpdyy = - pi**2 * rho * s * sin ( t ) * cos ( pi * x ) * sin ( pi * y )

  us = dudt + u * dudx + v * dudy + dpdx / rho - nu * ( dudxx + dudyy )
  vs = dvdt + u * dvdx + v * dvdy + dpdy / rho - nu * ( dvdxx + dvdyy )
  ps = dudx + dvdy

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
subroutine grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

!*****************************************************************************80
!
!! GRID_2D returns a regular 2D grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) X_NUM, the number of X values to use.
!
!    real ( kind = 8 ) X_LO, X_HI, the range of X values.
!
!    integer ( kind = 4 ) Y_NUM, the number of Y values to use.
!
!    real ( kind = 8 ) Y_LO, Y_HI, the range of Y values.
!
!  Output:
!
!    real ( kind = 8 ) X(X_NUM,Y_NUM), Y(X_NUM,Y_NUM), 
!    the coordinates of the grid.
!
  implicit none

  integer ( kind = 4 ) x_num
  integer ( kind = 4 ) y_num

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) x(x_num,y_num)
  real ( kind = 8 ) x_hi
  real ( kind = 8 ) x_lo
  real ( kind = 8 ) xi
  real ( kind = 8 ) y(x_num,y_num)
  real ( kind = 8 ) y_hi
  real ( kind = 8 ) y_lo
  real ( kind = 8 ) yj

  if ( x_num == 1 ) then
    x(1:x_num,1:y_num) = ( x_lo + x_hi ) / 2.0D+00
  else
    do i = 1, x_num
      xi = ( real ( x_num - i,     kind = 8 ) * x_lo   &
           + real (         i - 1, kind = 8 ) * x_hi ) &
           / real ( x_num     - 1, kind = 8 )
      x(i,1:y_num) = xi
    end do
  end if

  if ( y_num == 1 ) then
    y(1:x_num,1:y_num) = ( y_lo + y_hi ) / 2.0D+00
  else
    do j = 1, y_num
      yj = ( real ( y_num - j,     kind = 8 ) * y_lo   &
           + real (         j - 1, kind = 8 ) * y_hi ) &
           / real ( y_num     - 1, kind = 8 )
      y(1:x_num,j) = yj
    end do
  end if

  return
end
subroutine ns2de_gnuplot ( header, n, x, y, u, v, p, s )

!*****************************************************************************80
!
!! NS2DE_GNUPLOT writes the Navier-Stokes solution field to files for GNUPLOT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    character ( len = * ) HEADER, a header to be used to name the files.
!
!    integer ( kind = 4 ) N, the number of evaluation points.
!
!    real ( kind = 8 ) X(N), Y(N), the evaluation points.
!
!    real ( kind = 8 ) U(N), V(N), P(N), the velocity and pressure.
!
!    real ( kind = 8 ) S, a scale factor for the velocity vectors.
!
  implicit none

  integer ( kind = 4 ) n

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = * ) header
  integer ( kind = 4 ) i
  real ( kind = 8 ) p(n)
  character ( len = 255 ) plot_filename
  real ( kind = 8 ) s
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Write the data file.
!
  data_filename = trim ( header ) // '_data.txt'

  call get_unit ( data_unit )

  open ( unit = data_unit, file = data_filename, status = 'replace' )

  do i = 1, n
    write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      x(i), y(i), u(i), v(i), s * u(i), s * v(i), p(i)
  end do

  close ( unit = data_unit )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Data written to "' // trim ( data_filename ) // '".'
!
!  Write the command file.
!
  command_filename = trim ( header ) // '_commands.txt'
  call get_unit ( command_unit )

  plot_filename = trim ( header ) // '.png'

  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '#  ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '#  Add titles and labels.'
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set title "Navier-Stokes velocity field"'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '#  Add grid lines.'
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set size ratio -1'
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '#  Timestamp the plot.'
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2:5:6 with vectors \'
  write ( command_unit, '(a)' ) '  head filled lt 2 linecolor rgb "blue"'
  write ( command_unit, '(a)' ) 'quit'

  close ( unit = command_unit )

  write ( *, '(a)' ) '  Commands written to "' // &
    trim ( command_filename ) // '".'

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

  r8vec_norm_l2 = sqrt ( sum ( a(1:n)**2 ) )

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
!    integer ( kind = 4 ) SEED, the updated seed value.
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
subroutine resid_gms ( nu, rho, n, x, y, t, ur, vr, pr )

!*****************************************************************************80
!
!! resid_gms evaluates the residuals of the GMS flow.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the fluid density.
!
!    integer( kind = 4 ) N, the number of nodes.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of nodes.
!
!    real ( kind = 8 ) T, the current time.
!
!  Output:
!
!    real ( kind = 8 ) UR(N), VR(N), PR(N), the residuals.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) u(n)
  real ( kind = 8 ) dudt(n)
  real ( kind = 8 ) dudx(n)
  real ( kind = 8 ) dudxx(n)
  real ( kind = 8 ) dudy(n)
  real ( kind = 8 ) dudyy(n)
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) us(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) dvdt(n)
  real ( kind = 8 ) dvdx(n)
  real ( kind = 8 ) dvdxx(n)
  real ( kind = 8 ) dvdy(n)
  real ( kind = 8 ) dvdyy(n)
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) vs(n)
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) dpdt(n)
  real ( kind = 8 ) dpdx(n)
  real ( kind = 8 ) dpdxx(n)
  real ( kind = 8 ) dpdy(n)
  real ( kind = 8 ) dpdyy(n)
  real ( kind = 8 ) pr(n)
  real ( kind = 8 ) ps(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  call all_gms ( nu, rho, n, x, y, t, &
    u, dudt, dudx, dudxx, dudy, dudyy, us, &
    v, dvdt, dvdx, dvdxx, dvdy, dvdyy, vs, &
    p, dpdt, dpdx, dpdxx, dpdy, dpdyy, ps )

  ur = dudt + u * dudx + v * dudy + dpdx / rho - nu * ( dudxx + dudyy ) - us
  vr = dvdt + u * dvdx + v * dvdy + dpdy / rho - nu * ( dvdxx + dvdyy ) - vs
  pr = dudx + dvdy - ps

  return
end
subroutine resid_lukas ( nu, rho, n, x, y, t, ur, vr, pr )

!*****************************************************************************80
!
!! RESID_LUKAS returns Lukas Bystricky residuals.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 March 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of evaluation points.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    real ( kind = 8 ) T, the time coordinate or coordinates.
!
!  Output:
!
!    real ( kind = 8 ) UR(N), VR(N), PR(N), the residuals in the U, 
!    V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) dpdx(n)
  real ( kind = 8 ) dpdy(n)
  real ( kind = 8 ) dudt(n)
  real ( kind = 8 ) dudx(n)
  real ( kind = 8 ) dudxx(n)
  real ( kind = 8 ) dudy(n)
  real ( kind = 8 ) dudyy(n)
  real ( kind = 8 ) dvdt(n)
  real ( kind = 8 ) dvdx(n)
  real ( kind = 8 ) dvdxx(n)
  real ( kind = 8 ) dvdy(n)
  real ( kind = 8 ) dvdyy(n)
  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) pr(n)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Get the right hand side functions.
!
  call rhs_lukas ( nu, rho, n, x, y, t, f, g, h )
!
!  Form the functions and derivatives for the left hand side.
!
  u(1:n) = - cos ( r8_pi * x(1:n) ) / r8_pi
  dudt(1:n) = 0.0D+00
  dudx(1:n) = sin ( r8_pi * x(1:n) )
  dudxx(1:n) = r8_pi * cos ( r8_pi * x(1:n) )
  dudy(1:n) = 0.0D+00
  dudyy(1:n) = 0.0D+00

  v(1:n) = - y(1:n) * sin ( r8_pi * x(1:n) )
  dvdt(1:n) = 0.0D+00
  dvdx(1:n) = - r8_pi * y(1:n) * cos ( r8_pi * x(1:n) )
  dvdxx(1:n) = + r8_pi ** 2 * y(1:n) * sin ( r8_pi * x(1:n) )
  dvdy(1:n) = - sin ( r8_pi * x(1:n) )
  dvdyy(1:n) = 0.0D+00

  p(1:n) = 0.0D+00
  dpdx(1:n) = 0.0D+00
  dpdy(1:n) = 0.0D+00
!
!  Evaluate the residuals.
!
  ur(1:n) = dudt(1:n) + u(1:n) * dudx(1:n) + v(1:n) * dudy(1:n) &
    + ( 1.0D+00 / rho ) * dpdx(1:n) - nu * ( dudxx(1:n) + dudyy(1:n) ) - f(1:n)

  vr(1:n) = dvdt(1:n) + u(1:n) * dvdx(1:n) + v(1:n) * dvdy(1:n) &
    + ( 1.0D+00 / rho ) * dpdy(1:n) - nu * ( dvdxx(1:n) + dvdyy(1:n) ) - g(1:n)

  pr(1:n) = dudx(1:n) + dvdy(1:n) - h(1:n)

  return
end
subroutine resid_poiseuille ( nu, rho, n, x, y, t, ur, vr, pr )

!*****************************************************************************80
!
!! RESID_POISEUILLE returns Poiseuille flow residuals.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of evaluation points.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    real ( kind = 8 ) T, the time coordinate or coordinates.
!
!  Output:
!
!    real ( kind = 8 ) UR(N), VR(N), PR(N), the residuals in the U, 
!    V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) dpdx(n)
  real ( kind = 8 ) dpdy(n)
  real ( kind = 8 ) dudt(n)
  real ( kind = 8 ) dudx(n)
  real ( kind = 8 ) dudxx(n)
  real ( kind = 8 ) dudy(n)
  real ( kind = 8 ) dudyy(n)
  real ( kind = 8 ) dvdt(n)
  real ( kind = 8 ) dvdx(n)
  real ( kind = 8 ) dvdxx(n)
  real ( kind = 8 ) dvdy(n)
  real ( kind = 8 ) dvdyy(n)
  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) pr(n)
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Get the right hand side functions.
!
  call rhs_poiseuille ( nu, rho, n, x, y, t, f, g, h )
!
!  Form the functions and derivatives for the left hand side.
!
  u(1:n) = 1.0 - y(1:n) ** 2
  dudt(1:n) = 0.0D+00
  dudx(1:n) = 0.0D+00
  dudxx(1:n) = 0.0D+00
  dudy(1:n) = - 2.0D+00 * y(1:n)
  dudyy(1:n) = - 2.0D+00

  v(1:n) = 0.0D+00
  dvdt(1:n) = 0.0D+00
  dvdx(1:n) = 0.0D+00
  dvdxx(1:n) = 0.0D+00
  dvdy(1:n) = 0.0D+00
  dvdyy(1:n) = 0.0D+00

  p(1:n) = - 2.0D+00 * rho * nu * x(1:n)
  dpdx(1:n) = - 2.0D+00 * rho * nu
  dpdy(1:n) = 0.0D+00
!
!  Evaluate the residuals.
!
  ur(1:n) = dudt(1:n) + u(1:n) * dudx(1:n) + v(1:n) * dudy(1:n) &
    + ( 1.0D+00 / rho ) * dpdx(1:n) - nu * ( dudxx(1:n) + dudyy(1:n) ) - f(1:n)

  vr(1:n) = dvdt(1:n) + u(1:n) * dvdx(1:n) + v(1:n) * dvdy(1:n) &
    + ( 1.0D+00 / rho ) * dpdy(1:n) - nu * ( dvdxx(1:n) + dvdyy(1:n) ) - g(1:n)

  pr(1:n) = dudx(1:n) + dvdy(1:n) - h(1:n)

  return
end
subroutine resid_spiral ( nu, rho, n, x, y, t, ur, vr, pr )

!*****************************************************************************80
!
!! RESID_SPIRAL evaluates Sprial residuals.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Maxim Olshanskii, Leo Rebholz,
!    Application of barycenter refined meshes in linear elasticity
!    and incompressible fluid dynamics,
!    ETNA: Electronic Transactions in Numerical Analysis, 
!    Volume 38, pages 258-274, 2011.
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of nodes.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of nodes.
!
!    real ( kind = 8 ) T, the current time.
!
!    real ( kind = 8 ) U(N), V(N), the X and Y velocity.
!
!    real ( kind = 8 ) P(N), the pressure.
!
!  Output:
!
!    real ( kind = 8 ) UR(N), the U-equation residual.
!
!    real ( kind = 8 ) VR(N), the V-equation residual.
!
!    real ( kind = 8 ) PR(N), the P-equation residual.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) dpdx(n)
  real ( kind = 8 ) dpdy(n)
  real ( kind = 8 ) dudt(n)
  real ( kind = 8 ) dudx(n)
  real ( kind = 8 ) dudxx(n)
  real ( kind = 8 ) dudy(n)
  real ( kind = 8 ) dudyy(n)
  real ( kind = 8 ) dvdt(n)
  real ( kind = 8 ) dvdx(n)
  real ( kind = 8 ) dvdxx(n)
  real ( kind = 8 ) dvdy(n)
  real ( kind = 8 ) dvdyy(n)
  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) pr(n)
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Get the right hand side functions.
!
  call rhs_spiral ( nu, rho, n, x, y, t, f, g, h )
!
!  Form the functions and derivatives for the left hand side.
!
  u(1:n) = ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( x(1:n) ** 4 - 2.0D+00 * x(1:n) ** 3 + x(1:n) ** 2 ) &
    * ( 2.0D+00 * y(1:n) ** 3 - 3.0D+00 * y(1:n) ** 2 + y(1:n) )

  dudt(1:n) = nu * 2.0D+00 &
    * ( x(1:n) ** 4 - 2.0D+00 * x(1:n) ** 3 + x(1:n) ** 2 ) &
    * ( 2.0D+00 * y(1:n) ** 3  - 3.0D+00 * y(1:n) ** 2 + y(1:n) )

  dudx(1:n) = ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 4.0D+00 * x(1:n) ** 3 - 6.0D+00 * x(1:n) ** 2 + 2.0D+00 * x(1:n) ) &
    * ( 2.0D+00 * y(1:n) ** 3 - 3.0D+00 * y(1:n) ** 2 + y(1:n) )

  dudxx(1:n) = ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 12.0D+00 * x(1:n) ** 2 - 12.0D+00 * x(1:n) + 2.0D+00 ) &
    * ( 2.0D+00 * y(1:n) ** 3 - 3.0D+00 * y(1:n) ** 2 + y(1:n) )

  dudy(1:n) = ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( x(1:n) ** 4 - 2.0D+00 * x(1:n) ** 3 + x(1:n) ** 2 ) &
    * ( 6.0D+00 * y(1:n) ** 2  - 6.0D+00 * y(1:n) + 1.0D+00 )

  dudyy(1:n) = ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( x(1:n) ** 4 - 2.0D+00 * x(1:n) ** 3 + x(1:n) ** 2 ) &
    * ( 12.0D+00 * y(1:n) - 6.0D+00 )

  v(1:n) = - ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 2.0D+00 * x(1:n) ** 3 - 3.0D+00 * x(1:n) ** 2 + x(1:n) ) &
    * ( y(1:n) ** 4 - 2.0D+00 * y(1:n) ** 3 + y(1:n) ** 2 )

  dvdt(1:n) = - nu * 2.0D+00 &
    * ( 2.0D+00 * x(1:n) ** 3 - 3.0D+00 * x(1:n) ** 2 + x(1:n) ) &
    * ( y(1:n) ** 4 - 2.0D+00 * y(1:n) ** 3 + y(1:n) ** 2 )

  dvdx(1:n) = - ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 6.0D+00 * x(1:n) ** 2 - 6.0D+00 * x(1:n) + 1.0D+00 ) &
    * ( y(1:n) ** 4 - 2.0D+00 * y(1:n) ** 3 + y(1:n) ** 2 )

  dvdxx(1:n) = - ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 12.0D+00 * x(1:n) - 6.0D+00 ) &
    * ( y(1:n) ** 4 - 2.0D+00 * y(1:n) ** 3 + y(1:n) ** 2 )

  dvdy(1:n) = - ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 2.0D+00 * x(1:n) ** 3 - 3.0D+00 * x(1:n) ** 2 + x(1:n) ) &
    * ( 4.0D+00 * y(1:n) ** 3 - 6.0D+00 * y(1:n) ** 2 + 2.0D+00 * y(1:n) )

  dvdyy(1:n) = - ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 2.0D+00 * x(1:n) ** 3 - 3.0D+00 * x(1:n) ** 2 + x(1:n) ) &
    * ( 12.0D+00 * y(1:n) ** 2 - 12.0D+00 * y(1:n) + 2.0D+00 )

  p(1:n) = rho * y(1:n)
  dpdx(1:n) = 0.0D+00
  dpdy(1:n) = rho
!
!  Evaluate the residuals.
!
  ur(1:n) = dudt(1:n) + u(1:n) * dudx(1:n) + v(1:n) * dudy(1:n) &
    + ( 1.0D+00 / rho ) * dpdx(1:n) - nu * ( dudxx(1:n) + dudyy(1:n) ) - f(1:n)

  vr(1:n) = dvdt(1:n) + u(1:n) * dvdx(1:n) + v(1:n) * dvdy(1:n) &
    + ( 1.0D+00 / rho ) * dpdy(1:n) - nu * ( dvdxx(1:n) + dvdyy(1:n) ) - g(1:n)

  pr(1:n) = dudx(1:n) + dvdy(1:n) - h(1:n)

  return
end
subroutine resid_taylor ( nu, rho, n, x, y, t, ur, vr, pr )

!*****************************************************************************80
!
!! RESID_TAYLOR returns Taylor residuals.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Geoffrey Taylor,
!    On the decay of vortices in a viscous fluid,
!    Philosophical Magazine,
!    Volume 46, 1923, pages 671-674.
!
!    Geoffrey Taylor, A E Green,
!    Mechanism for the production of small eddies from large ones,
!    Proceedings of the Royal Society of London, 
!    Series A, Volume 158, 1937, pages 499-521.
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of evaluation points.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    real ( kind = 8 ) T, the time coordinate or coordinates.
!
!  Output:
!
!    real ( kind = 8 ) UR(N), VR(N), PR(N), the residuals in the U, 
!    V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) dpdx(n)
  real ( kind = 8 ) dpdy(n)
  real ( kind = 8 ) dudt(n)
  real ( kind = 8 ) dudx(n)
  real ( kind = 8 ) dudxx(n)
  real ( kind = 8 ) dudy(n)
  real ( kind = 8 ) dudyy(n)
  real ( kind = 8 ) dvdt(n)
  real ( kind = 8 ) dvdx(n)
  real ( kind = 8 ) dvdxx(n)
  real ( kind = 8 ) dvdy(n)
  real ( kind = 8 ) dvdyy(n)
  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) pr(n)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Get the right hand side functions.
!
  call rhs_taylor ( nu, rho, n, x, y, t, f, g, h )
!
!  Form the functions and derivatives for the left hand side.
!
  u(1:n)  =  -                          &
      cos ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )
  dudx(1:n) =                       r8_pi &
    * sin ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )
  dudxx(1:n) =              r8_pi * r8_pi &
    * cos ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )
  dudy(1:n) =  -                    r8_pi &
    * cos ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )
  dudyy(1:n) =              r8_pi * r8_pi &
    * cos ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )
  dudt(1:n) =  + 2.0 * nu * r8_pi * r8_pi &
    * cos ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )

  v(1:n)  =                             &
      sin ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )
  dvdx(1:n) =                       r8_pi &
    * cos ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )
  dvdxx(1:n) = -            r8_pi * r8_pi &
    * sin ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )
  dvdy(1:n) =  -                    r8_pi &
    * sin ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )
  dvdyy(1:n) = -            r8_pi * r8_pi &
    * sin ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )
  dvdt(1:n) =  - 2.0 * nu * r8_pi * r8_pi &
    * sin ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )

  p(1:n) =   - 0.25D+00 * rho * &
    ( cos ( 2.0D+00 * r8_pi * x(1:n) ) + cos ( 2.0D+00 * r8_pi * y(1:n) ) )
  dpdx(1:n) =  + 0.5D+00  * rho * r8_pi * sin ( 2.0D+00 * r8_pi * x(1:n) )
  dpdy(1:n) =  + 0.5D+00  * rho * r8_pi * sin ( 2.0D+00 * r8_pi * y(1:n) )
!
!  Time scaling.
!
  u(1:n)     = u(1:n)     * exp ( - 2.0 * r8_pi ** 2 * nu * t )
  dudx(1:n)  = dudx(1:n)  * exp ( - 2.0 * r8_pi ** 2 * nu * t )
  dudxx(1:n) = dudxx(1:n) * exp ( - 2.0 * r8_pi ** 2 * nu * t )
  dudy(1:n)  = dudy(1:n)  * exp ( - 2.0 * r8_pi ** 2 * nu * t )
  dudyy(1:n) = dudyy(1:n) * exp ( - 2.0 * r8_pi ** 2 * nu * t )
  dudt(1:n)  = dudt(1:n)  * exp ( - 2.0 * r8_pi ** 2 * nu * t )

  v(1:n)     = v(1:n)     * exp ( - 2.0 * r8_pi ** 2 * nu * t )
  dvdx(1:n)  = dvdx(1:n)  * exp ( - 2.0 * r8_pi ** 2 * nu * t )
  dvdxx(1:n) = dvdxx(1:n) * exp ( - 2.0 * r8_pi ** 2 * nu * t )
  dvdy(1:n)  = dvdy(1:n)  * exp ( - 2.0 * r8_pi ** 2 * nu * t )
  dvdyy(1:n) = dvdyy(1:n) * exp ( - 2.0 * r8_pi ** 2 * nu * t )
  dvdt(1:n)  = dvdt(1:n)  * exp ( - 2.0 * r8_pi ** 2 * nu * t )

  p(1:n) =     p(1:n)     * exp ( - 4.0 * r8_pi ** 2 * nu * t )
  dpdx(1:n) =  dpdx(1:n)  * exp ( - 4.0 * r8_pi ** 2 * nu * t )
  dpdy(1:n) =  dpdy(1:n)  * exp ( - 4.0 * r8_pi ** 2 * nu * t )
!
!  Evaluate the residuals.
!
  ur(1:n) = dudt(1:n) + u(1:n) * dudx(1:n) + v(1:n) * dudy(1:n) &
    + ( 1.0D+00 / rho ) * dpdx(1:n) - nu * ( dudxx(1:n) + dudyy(1:n) ) - f(1:n)

  vr(1:n) = dvdt(1:n) + u(1:n) * dvdx(1:n) + v(1:n) * dvdy(1:n) &
    + ( 1.0D+00 / rho ) * dpdy(1:n) - nu * ( dvdxx(1:n) + dvdyy(1:n) ) - g(1:n)

  pr(1:n) = dudx(1:n) + dvdy(1:n) - h(1:n)

  return
end
subroutine resid_vortex ( nu, rho, n, x, y, t, ur, vr, pr )

!*****************************************************************************80
!
!! RESID_VORTEX returns Vortex residuals.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of evaluation points.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    real ( kind = 8 ) T, the time coordinate or coordinates.
!
!  Output:
!
!    real ( kind = 8 ) UR(N), VR(N), PR(N), the residuals in the U, 
!    V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) dpdx(n)
  real ( kind = 8 ) dpdy(n)
  real ( kind = 8 ) dudt(n)
  real ( kind = 8 ) dudx(n)
  real ( kind = 8 ) dudxx(n)
  real ( kind = 8 ) dudy(n)
  real ( kind = 8 ) dudyy(n)
  real ( kind = 8 ) dvdt(n)
  real ( kind = 8 ) dvdx(n)
  real ( kind = 8 ) dvdxx(n)
  real ( kind = 8 ) dvdy(n)
  real ( kind = 8 ) dvdyy(n)
  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) pr(n)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) ur(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) vr(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)
!
!  Get the right hand side functions.
!
  call rhs_vortex ( nu, rho, n, x, y, t, f, g, h )
!
!  Form the functions and derivatives for the left hand side.
!
  u(1:n)  =  -                          &
      cos ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )
  dudx(1:n) =                       r8_pi &
    * sin ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )
  dudxx(1:n) =              r8_pi * r8_pi &
    * cos ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )
  dudy(1:n) =  -                    r8_pi &
    * cos ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )
  dudyy(1:n) =              r8_pi * r8_pi &
    * cos ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )
  dudt(1:n) =  + 2.0 * nu * r8_pi * r8_pi &
    * cos ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )

  v(1:n)  =                             &
      sin ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )
  dvdx(1:n) =                       r8_pi &
    * cos ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )
  dvdxx(1:n) = -            r8_pi * r8_pi &
    * sin ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )
  dvdy(1:n) =  -                    r8_pi &
    * sin ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )
  dvdyy(1:n) = -            r8_pi * r8_pi &
    * sin ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )
  dvdt(1:n) =  - 2.0 * nu * r8_pi * r8_pi &
    * sin ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )

  p(1:n) =   - 0.25D+00 * rho * &
    ( cos ( 2.0D+00 * r8_pi * x(1:n) ) + cos ( 2.0D+00 * r8_pi * y(1:n) ) )
  dpdx(1:n) =  + 0.5D+00  * rho * r8_pi * sin ( 2.0D+00 * r8_pi * x(1:n) )
  dpdy(1:n) =  + 0.5D+00  * rho * r8_pi * sin ( 2.0D+00 * r8_pi * y(1:n) )
!
!  Evaluate the residuals.
!
  ur(1:n) = dudt(1:n) + u(1:n) * dudx(1:n) + v(1:n) * dudy(1:n) &
    + ( 1.0D+00 / rho ) * dpdx(1:n) - nu * ( dudxx(1:n) + dudyy(1:n) ) - f(1:n)

  vr(1:n) = dvdt(1:n) + u(1:n) * dvdx(1:n) + v(1:n) * dvdy(1:n) &
    + ( 1.0D+00 / rho ) * dpdy(1:n) - nu * ( dvdxx(1:n) + dvdyy(1:n) ) - g(1:n)

  pr(1:n) = dudx(1:n) + dvdy(1:n) - h(1:n)

  return
end
subroutine rhs_gms ( nu, rho, n, x, y, t, us, vs, ps )

!*****************************************************************************80
!
!! rhs_gms evaluates the source terms of the GMS flow.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the fluid density.
!
!    integer ( kind = 4 ) N, the number of nodes.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of nodes.
!
!    real ( kind = 8 ) T, the current time.
!
!  Output:
!
!    real ( kind = 8 ) US(N), VS(N), PS(N), the source terms.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) u(n)
  real ( kind = 8 ) dudt(n)
  real ( kind = 8 ) dudx(n)
  real ( kind = 8 ) dudxx(n)
  real ( kind = 8 ) dudy(n)
  real ( kind = 8 ) dudyy(n)
  real ( kind = 8 ) us(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) dvdt(n)
  real ( kind = 8 ) dvdx(n)
  real ( kind = 8 ) dvdxx(n)
  real ( kind = 8 ) dvdy(n)
  real ( kind = 8 ) dvdyy(n)
  real ( kind = 8 ) vs(n)
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) dpdt(n)
  real ( kind = 8 ) dpdx(n)
  real ( kind = 8 ) dpdxx(n)
  real ( kind = 8 ) dpdy(n)
  real ( kind = 8 ) dpdyy(n)
  real ( kind = 8 ) ps(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  call all_gms ( nu, rho, n, x, y, t, &
    u, dudt, dudx, dudxx, dudy, dudyy, us, &
    v, dvdt, dvdx, dvdxx, dvdy, dvdyy, vs, &
    p, dpdt, dpdx, dpdxx, dpdy, dpdyy, ps )

  return
end
subroutine rhs_lukas ( nu, rho, n, x, y, t, f, g, h )

!*****************************************************************************80
!
!! RHS_LUKAS evaluates the Lukas Bystricky right hand sides.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 March 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of nodes.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of nodes.
!
!    real ( kind = 8 ) T, the current time.
!
!  Output:
!
!    real ( kind = 8 ) F(N), G(N), H(N), the right hand sides.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) dpdx(n)
  real ( kind = 8 ) dpdy(n)
  real ( kind = 8 ) dudt(n)
  real ( kind = 8 ) dudx(n)
  real ( kind = 8 ) dudxx(n)
  real ( kind = 8 ) dudy(n)
  real ( kind = 8 ) dudyy(n)
  real ( kind = 8 ) dvdt(n)
  real ( kind = 8 ) dvdx(n)
  real ( kind = 8 ) dvdxx(n)
  real ( kind = 8 ) dvdy(n)
  real ( kind = 8 ) dvdyy(n)
  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  call r8_fake_use ( t )

  u(1:n) = - cos ( r8_pi * x(1:n) ) / r8_pi
  dudt(1:n) = 0.0D+00
  dudx(1:n) = sin ( r8_pi * x(1:n) )
  dudxx(1:n) = r8_pi * cos ( r8_pi * x(1:n) )
  dudy(1:n) = 0.0D+00
  dudyy(1:n) = 0.0D+00

  v(1:n) = - y(1:n) * sin ( r8_pi * x(1:n) )
  dvdt(1:n) = 0.0D+00
  dvdx(1:n) = - r8_pi * y(1:n) * cos ( r8_pi * x(1:n) )
  dvdxx(1:n) = + r8_pi ** 2 * y(1:n) * sin ( r8_pi * x(1:n) )
  dvdy(1:n) = - sin ( r8_pi * x(1:n) )
  dvdyy(1:n) = 0.0D+00

  p(1:n) = 0.0D+00
  dpdx(1:n) = 0.0D+00
  dpdy(1:n) = 0.0D+00

  f(1:n) = dudt(1:n) - nu * ( dudxx(1:n) + dudyy(1:n) ) &
    + u(1:n) * dudx(1:n) + v(1:n) * dudy(1:n) + dpdx(1:n) / rho

  g(1:n) = dvdt(1:n) - nu * ( dvdxx(1:n) + dvdyy(1:n) ) &
    + u(1:n) * dvdx(1:n) + v(1:n) * dvdy(1:n) + dpdy(1:n) / rho

  h(1:n) = dudx(1:n) + dvdy(1:n)

  return
end
subroutine rhs_poiseuille ( nu, rho, n, x, y, t, f, g, h )

!*****************************************************************************80
!
!! RHS_POISEUILLE returns Poiseuille right hand sides.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of evaluation points.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    real ( kind = 8 ) T, the time coordinate or coordinates.
!
!  Output:
!
!    real ( kind = 8 ) F(N), G(N), H(N), the right hand sides of 
!    the U, V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  call r8_fake_use ( nu )
  call r8_fake_use ( rho )
  call r8_fake_use ( t )
  call r8_fake_use ( x(1) )
  call r8_fake_use ( y(1) )

  f(1:n) = 0.0D+00
  g(1:n) = 0.0D+00
  h(1:n) = 0.0D+00

  return
end
subroutine rhs_spiral ( nu, rho, n, x, y, t, f, g, h )

!*****************************************************************************80
!
!! RHS_SPIRAL evaluates Spiral right hand sides.
!
!  Discussion:
!
!    The right hand side is artificially determined by the requirement
!    that the specified values of U, V and P satisfy the discretized
!    Navier Stokes and continuity equations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Maxim Olshanskii, Leo Rebholz,
!    Application of barycenter refined meshes in linear elasticity
!    and incompressible fluid dynamics,
!    ETNA: Electronic Transactions in Numerical Analysis, 
!    Volume 38, pages 258-274, 2011.
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of nodes.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of nodes.
!
!    real ( kind = 8 ) T, the current time.
!
!  Output:
!
!    real ( kind = 8 ) F(N), G(N), H(N), the right hand sides.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) dpdx(n)
  real ( kind = 8 ) dpdy(n)
  real ( kind = 8 ) dudt(n)
  real ( kind = 8 ) dudx(n)
  real ( kind = 8 ) dudxx(n)
  real ( kind = 8 ) dudy(n)
  real ( kind = 8 ) dudyy(n)
  real ( kind = 8 ) dvdt(n)
  real ( kind = 8 ) dvdx(n)
  real ( kind = 8 ) dvdxx(n)
  real ( kind = 8 ) dvdy(n)
  real ( kind = 8 ) dvdyy(n)
  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  u(1:n) = ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( x(1:n) ** 4 - 2.0D+00 * x(1:n) ** 3 + x(1:n) ** 2 ) &
    * ( 2.0D+00 * y(1:n) ** 3 - 3.0D+00 * y(1:n) ** 2 + y(1:n) )

  dudt(1:n) = nu * 2.0D+00 &
    * ( x(1:n) ** 4 - 2.0D+00 * x(1:n) ** 3 + x(1:n) ** 2 ) &
    * ( 2.0D+00 * y(1:n) ** 3  - 3.0D+00 * y(1:n) ** 2 + y(1:n) )

  dudx(1:n) = ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 4.0D+00 * x(1:n) ** 3 - 6.0D+00 * x(1:n) ** 2 + 2.0D+00 * x(1:n) ) &
    * ( 2.0D+00 * y(1:n) ** 3 - 3.0D+00 * y(1:n) ** 2 + y(1:n) )

  dudxx(1:n) = ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 12.0D+00 * x(1:n) ** 2 - 12.0D+00 * x(1:n) + 2.0D+00 ) &
    * ( 2.0D+00 * y(1:n) ** 3 - 3.0D+00 * y(1:n) ** 2 + y(1:n) )

  dudy(1:n) = ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( x(1:n) ** 4 - 2.0D+00 * x(1:n) ** 3 + x(1:n) ** 2 ) &
    * ( 6.0D+00 * y(1:n) ** 2  - 6.0D+00 * y(1:n) + 1.0D+00 )

  dudyy(1:n) = ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( x(1:n) ** 4 - 2.0D+00 * x(1:n) ** 3 + x(1:n) ** 2 ) &
    * ( 12.0D+00 * y(1:n) - 6.0D+00 )

  v(1:n) = - ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 2.0D+00 * x(1:n) ** 3 - 3.0D+00 * x(1:n) ** 2 + x(1:n) ) &
    * ( y(1:n) ** 4 - 2.0D+00 * y(1:n) ** 3 + y(1:n) ** 2 )

  dvdt(1:n) = - nu * 2.0D+00 &
    * ( 2.0D+00 * x(1:n) ** 3 - 3.0D+00 * x(1:n) ** 2 + x(1:n) ) &
    * ( y(1:n) ** 4 - 2.0D+00 * y(1:n) ** 3 + y(1:n) ** 2 )

  dvdx(1:n) = - ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 6.0D+00 * x(1:n) ** 2 - 6.0D+00 * x(1:n) + 1.0D+00 ) &
    * ( y(1:n) ** 4 - 2.0D+00 * y(1:n) ** 3 + y(1:n) ** 2 )

  dvdxx(1:n) = - ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 12.0D+00 * x(1:n) - 6.0D+00 ) &
    * ( y(1:n) ** 4 - 2.0D+00 * y(1:n) ** 3 + y(1:n) ** 2 )

  dvdy(1:n) = - ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 2.0D+00 * x(1:n) ** 3 - 3.0D+00 * x(1:n) ** 2 + x(1:n) ) &
    * ( 4.0D+00 * y(1:n) ** 3 - 6.0D+00 * y(1:n) ** 2 + 2.0D+00 * y(1:n) )

  dvdyy(1:n) = - ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * ( 2.0D+00 * x(1:n) ** 3 - 3.0D+00 * x(1:n) ** 2 + x(1:n) ) &
    * ( 12.0D+00 * y(1:n) ** 2 - 12.0D+00 * y(1:n) + 2.0D+00 )

  p(1:n) = rho * y(1:n)
  dpdx(1:n) = 0.0D+00
  dpdy(1:n) = rho

  f(1:n) = dudt(1:n) - nu * ( dudxx(1:n) + dudyy(1:n) ) &
    + u(1:n) * dudx(1:n) + v(1:n) * dudy(1:n) + dpdx(1:n) / rho

  g(1:n) = dvdt(1:n) - nu * ( dvdxx(1:n) + dvdyy(1:n) ) &
    + u(1:n) * dvdx(1:n) + v(1:n) * dvdy(1:n) + dpdy(1:n) / rho

  h(1:n) = dudx(1:n) + dvdy(1:n)

  return
end
subroutine rhs_taylor ( nu, rho, n, x, y, t, f, g, h )

!*****************************************************************************80
!
!! RHS_TAYLOR returns Taylor right hand sides.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Geoffrey Taylor,
!    On the decay of vortices in a viscous fluid,
!    Philosophical Magazine,
!    Volume 46, 1923, pages 671-674.
!
!    Geoffrey Taylor, A E Green,
!    Mechanism for the production of small eddies from large ones,
!    Proceedings of the Royal Society of London, 
!    Series A, Volume 158, 1937, pages 499-521.
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of evaluation points.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    real ( kind = 8 ) T, the time coordinate or coordinates.
!
!  Output:
!
!    real ( kind = 8 ) F(N), G(N), H(N), the right hand sides of 
!    the U, V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  call r8_fake_use ( nu )
  call r8_fake_use ( rho )
  call r8_fake_use ( t )
  call r8_fake_use ( x(1) )
  call r8_fake_use ( y(1) )

  f(1:n) = 0.0D+00
  g(1:n) = 0.0D+00
  h(1:n) = 0.0D+00

  return
end
subroutine rhs_vortex ( nu, rho, n, x, y, t, f, g, h )

!*****************************************************************************80
!
!! RHS_VORTEX returns Vortex right hand sides.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of evaluation points.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    real ( kind = 8 ) T, the time coordinate or coordinates.
!
!  Output:
!
!    real ( kind = 8 ) F(N), G(N), H(N), the right hand sides of 
!    the U, V and P equations.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) f(n)
  real ( kind = 8 ) g(n)
  real ( kind = 8 ) h(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  call r8_fake_use ( rho )
  call r8_fake_use ( t )

  f(1:n) = - 2.0 * nu * r8_pi * r8_pi &
    * cos ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) )
  g(1:n) =   2.0 * nu * r8_pi * r8_pi &
    * sin ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) )
  h(1:n) = 0.0D+00

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
subroutine uvp_gms ( nu, rho, n, x, y, t, u, v, p )

!*****************************************************************************80
!
!! uvp_gms returns velocity and pressure for the GMS flow.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2020
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jean-Luc Guermand, Peter Minev, Jie Shen,
!    An overview of projection methods for incompressible flows,
!    Computer methods in applied mechanics and engineering, 
!    Volume 105, pages 6011-6045, 2006.
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the fluid density.
!
!    integer( kind = 4 ) N, the number of nodes.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of nodes.
!
!    real ( kind = 8 ) T, the current time.
!
!  Output:
!
!    real ( kind = 8 ) U(N), V(N), the X and Y velocity.
!
!    real ( kind = 8 ) P(N), the pressure.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) u(n)
  real ( kind = 8 ) dudt(n)
  real ( kind = 8 ) dudx(n)
  real ( kind = 8 ) dudxx(n)
  real ( kind = 8 ) dudy(n)
  real ( kind = 8 ) dudyy(n)
  real ( kind = 8 ) us(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) dvdt(n)
  real ( kind = 8 ) dvdx(n)
  real ( kind = 8 ) dvdxx(n)
  real ( kind = 8 ) dvdy(n)
  real ( kind = 8 ) dvdyy(n)
  real ( kind = 8 ) vs(n)
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) dpdt(n)
  real ( kind = 8 ) dpdx(n)
  real ( kind = 8 ) dpdxx(n)
  real ( kind = 8 ) dpdy(n)
  real ( kind = 8 ) dpdyy(n)
  real ( kind = 8 ) ps(n)
  real ( kind = 8 ) nu
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  call all_gms ( nu, rho, n, x, y, t, &
    u, dudt, dudx, dudxx, dudy, dudyy, us, &
    v, dvdt, dvdx, dvdxx, dvdy, dvdyy, vs, &
    p, dpdt, dpdx, dpdxx, dpdy, dpdyy, ps )

  return
end
subroutine uvp_lukas ( nu, rho, n, x, y, t, u, v, p )

!*****************************************************************************80
!
!! UVP_LUKAS evaluates Lukas Bystricky's solution.
!
!  Discussion:
!
!    There is no time dependence.
!
!    The pressure is 0.
!
!    The preferred domain is the unit square.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 March 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of evaluation points.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    real ( kind = 8 ) T, the time coordinate or coordinates.
!
!  Output:
!
!    real ( kind = 8 ) U(N), V(N), P(N), the velocity components and
!    pressure at each of the points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  call r8_fake_use ( nu )
  call r8_fake_use ( rho )
  call r8_fake_use ( t )

  u(1:n) = - cos ( r8_pi * x(1:n) ) / r8_pi
  v(1:n) = - y(1:n) * sin ( r8_pi * x(1:n) )
  p(1:n) = 0.0D+00

  return
end
subroutine uvp_poiseuille ( nu, rho, n, x, y, t, u, v, p )

!*****************************************************************************80
!
!! UVP_POISEUILLE evaluates Poiseuille solutions.
!
!  Discussion:
!
!    There is no time dependence.
!
!    The vertical velocity is zero.
!
!    The preferred domain is a channel bounded by y = -1 and y = +1,
!    along which the boundary condition u = 0 and v = 0 will be satisfied.
!    A parabolic inflow may then be imposed along some line, such as x=0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of evaluation points.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    real ( kind = 8 ) T, the time coordinate or coordinates.
!
!  Output:
!
!    real ( kind = 8 ) U(N), V(N), P(N), the velocity components and
!    pressure at each of the points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  call r8_fake_use ( t )

  u(1:n) = 1.0 - y(1:n) ** 2
  v(1:n) = 0.0D+00
  p(1:n) = - 2.0D+00 * rho * nu * x(1:n)

  return
end
subroutine uvp_spiral ( nu, rho, n, x, y, t, u, v, p )

!*****************************************************************************80
!
!! UVP_SPIRAL returns Spiral solutions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 February 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Maxim Olshanskii, Leo Rebholz,
!    Application of barycenter refined meshes in linear elasticity
!    and incompressible fluid dynamics,
!    ETNA: Electronic Transactions in Numerical Analysis, 
!    Volume 38, pages 258-274, 2011.
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of nodes.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of nodes.
!
!    real ( kind = 8 ) T, the current time.
!
!  Output:
!
!    real ( kind = 8 ) U(N), V(N), the X and Y velocity.
!
!    real ( kind = 8 ) P(N), the pressure.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  u(1:n) = ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * x(1:n)**2 * ( x(1:n) - 1.0D+00 )**2 &
    * y(1:n) * ( 2.0D+00 * y(1:n) - 1.0D+00 ) * ( y(1:n) - 1.0D+00 )

  v(1:n) = - ( 1.0D+00 + nu * t ) * 2.0D+00 &
    * x(1:n) * ( 2.0D+00 * x(1:n) - 1.0D+00 ) * ( x(1:n) - 1.0D+00 )  &
    * y(1:n)**2 * ( y(1:n) - 1.0D+00 )**2

  p(1:n) = rho * y(1:n)

  return
end
subroutine uvp_taylor ( nu, rho, n, x, y, t, u, v, p )

!*****************************************************************************80
!
!! UVP_TAYLOR evaluates Taylor solutions.
!
!  Discussion:
!
!    This flow is known as a Taylor-Green vortex.
!
!    The given velocity and pressure fields are exact solutions for the 2D 
!    incompressible time-dependent Navier Stokes equations over any region.
!
!    To define a typical problem, one chooses a bounded spatial region 
!    and a starting time, and then imposes boundary and initial conditions
!    by referencing the exact solution appropriately.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Geoffrey Taylor,
!    On the decay of vortices in a viscous fluid,
!    Philosophical Magazine,
!    Volume 46, 1923, pages 671-674.
!
!    Geoffrey Taylor, A E Green,
!    Mechanism for the production of small eddies from large ones,
!    Proceedings of the Royal Society of London, 
!    Series A, Volume 158, 1937, pages 499-521.
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of evaluation points.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    real ( kind = 8 ) T, the time coordinate or coordinates.
!
!  Output:
!
!    real ( kind = 8 ) U(N), V(N), P(N), the velocity components and
!    pressure at each of the points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  u(1:n) = - cos ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) ) 
  v(1:n) =   sin ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) ) 
  p(1:n) = - 0.25D+00 * rho &
    * ( cos ( 2.0D+00 * r8_pi * x(1:n) ) + cos ( 2.0D+00 * r8_pi * y(1:n) ) )

  u(1:n) = u(1:n) * exp ( - 2.0D+00 * r8_pi ** 2 * nu * t )
  v(1:n) = v(1:n) * exp ( - 2.0D+00 * r8_pi ** 2 * nu * t )
  p(1:n) = p(1:n) * exp ( - 4.0D+00 * r8_pi ** 2 * nu * t )

  return
end
subroutine uvp_vortex ( nu, rho, n, x, y, t, u, v, p )

!*****************************************************************************80
!
!! UVP_VORTEX evaluates Vortex solutions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = 8 ) NU, the kinematic viscosity.
!
!    real ( kind = 8 ) RHO, the density.
!
!    integer ( kind = 4 ) N, the number of evaluation points.
!
!    real ( kind = 8 ) X(N), Y(N), the coordinates of the points.
!
!    real ( kind = 8 ) T, the time coordinate or coordinates.
!
!  Output:
!
!    real ( kind = 8 ) U(N), V(N), P(N), the velocity components and
!    pressure at each of the points.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) nu
  real ( kind = 8 ) p(n)
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) rho
  real ( kind = 8 ) t
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  call r8_fake_use ( nu )
  call r8_fake_use ( t )

  u(1:n) = - cos ( r8_pi * x(1:n) ) * sin ( r8_pi * y(1:n) ) 
  v(1:n) =   sin ( r8_pi * x(1:n) ) * cos ( r8_pi * y(1:n) ) 
  p(1:n) = - 0.25D+00 * rho &
    * ( cos ( 2.0D+00 * r8_pi * x(1:n) ) + cos ( 2.0D+00 * r8_pi * y(1:n) ) )

  return
end
