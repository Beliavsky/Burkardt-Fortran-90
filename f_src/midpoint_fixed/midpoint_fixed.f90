subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! get_unit() returns a free FORTRAN unit number.
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
subroutine midpoint_fixed ( dydt, tspan, y0, n, m, t, y )

!*****************************************************************************80
!
!! midpoint_fixed() uses a fixed-point midpoint method to solve an ODE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    external dydt: a subroutine that evaluates the right
!    hand side of the ODE.
!
!    real ( kind = 8 ) tspan(2): contains the initial and final times.
!
!    real ( kind = 8 ) y0(m): a column vector containing the initial condition.
!
!    integer ( kind = 4 ) n: the number of steps to take.
!
!    integer ( kind = 4 ) m: the number of variables.
!
!  Output:
!
!    real ( kind = 8 ) t(n+1), y(n+1,m): the times and solution values.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) dt
  external dydt
  real ( kind = 8 ) f(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) j
  real ( kind = 8 ) t(n+1)
  real ( kind = 8 ) theta
  real ( kind = 8 ) tm
  real ( kind = 8 ) tspan(2)
  real ( kind = 8 ) y(n+1,m)
  real ( kind = 8 ) y0(m)
  real ( kind = 8 ) ym(m)

  dt = ( tspan(2) - tspan(1) ) / n

  it_max = 10
  theta = 0.5D+00

  t(1) = tspan(1)
  y(1,1:m) = y0(1:m)

  do i = 1, n
    tm = t(i) + theta * dt 
    ym(1:m) = y(i,1:m)
    do j = 1, it_max
      call dydt ( tm, ym(1:m), f )
      ym(1:m) = y(i,1:m) + theta * dt * f(1:m)
    end do
    t(i+1) = t(i) + dt
    y(i+1,1:m) = (           1.0D+00 / theta ) * ym(1:m) &
               + ( 1.0D+00 - 1.0D+00 / theta ) * y(i,1:m)
  end do

  return
end
subroutine r8_fake_use ( x )

!*****************************************************************************80
!
!! r8_fake_use() pretends to use a variable.
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
!! r8vec_linspace() creates a vector of linearly spaced values.
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

