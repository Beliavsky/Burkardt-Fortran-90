program main

!*****************************************************************************80
!
!! fsolve_test tests fsolve().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'fsolve_test():'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test fsolve(), which solves systems of nonlinear equations.'

  call fsolve_test1 ( )
  call fsolve_test2 ( )
  call fsolve_test3 ( )
  call fsolve_test4 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'fsolve_test():'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine fsolve_test1 ( )

!*****************************************************************************80
!
!! fsolve_test1() tests fsolve() on a system of 1 equation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 1

  external f1
  real ( kind = 8 ) fx(n)
  integer ( kind = 4 ) info
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'fsolve_test1():'
  write ( *, '(a)' ) '  fsolve() solves a nonlinear system of 1 equation.'

  x(1:1) = (/ 0.0D+00 /)

  call f1 ( n, x, fx )      
  call r8vec2_print ( n, x, fx, '  Initial X and F(X)' )

  tol = 0.00001D+00

  call fsolve ( f1, n, x, fx, tol, info )

  write ( *, '(a)' ) ' '
  if ( info == 1 ) then
    write ( *, '(a)' ) '  Satisfactory computation.'
  else
    write ( *, '(a,i6)' ) '  Returned value of INFO = ', info
  end if

  call r8vec2_print ( n, x, fx, '  Final X and F(X)' )

  return
end
subroutine f1 ( n, x, fx )

!*****************************************************************************80
!
!! f1() evaluates a nonlinear system of 1 equation.
!
!  Discussion:
!
!    This is Kepler's equation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of variables.
!
!    real ( kind = 8 ) X(N), the variable values.
!
!  Output:
!
!    real ( kind = 8 ) FX(N), the function values at X.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) e
  real ( kind = 8 ) fx(n)
  real ( kind = 8 ) m
  real ( kind = 8 ) x(n)

  e = 0.8D+00
  m = 5.0D+00
  fx(1) = x(1) - m - e * sin ( x(1) )

  return
end
subroutine fsolve_test2 ( )

!*****************************************************************************80
!
!! fsolve_test2() tests fsolve() on a system of 2 equations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 2

  external f2
  real ( kind = 8 ) fx(n)
  integer ( kind = 4 ) info
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'fsolve_test2():'
  write ( *, '(a)' ) '  fsolve() solves a nonlinear system of 2 equations.'

  x(1:2) = (/ 3.0D+00, 0.0D+00 /)

  call f2 ( n, x, fx )      
  call r8vec2_print ( n, x, fx, '  Initial X and F(X)' )

  tol = 0.00001D+00

  call fsolve ( f2, n, x, fx, tol, info )

  write ( *, '(a)' ) ' '
  if ( info == 1 ) then
    write ( *, '(a)' ) '  Satisfactory computation.'
  else
    write ( *, '(a,i6)' ) '  Returned value of INFO = ', info
  end if

  call r8vec2_print ( n, x, fx, '  Final X and F(X)' )

  return
end
subroutine f2 ( n, x, fx )

!*****************************************************************************80
!
!! f2() evaluates a nonlinear system of 2 equations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of variables.
!
!    real ( kind = 8 ) X(N), the variable values.
!
!  Output:
!
!    real ( kind = 8 ) FX(N), the function values at X.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fx(n)
  real ( kind = 8 ) x(n)

  fx(1) = x(1) * x(1) - 10.0D+00 * x(1) + x(2) * x(2) + 8.0D+00
  fx(2) = x(1) * x(2) * x(2) + x(1) - 10.0D+00 * x(2) + 8.0D+00

  return
end
subroutine fsolve_test3 ( )

!*****************************************************************************80
!
!! fsolve_test3() tests fsolve() on a system of 4 equations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  external f3
  real ( kind = 8 ) fx(n)
  integer ( kind = 4 ) info
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'fsolve_test3():'
  write ( *, '(a)' ) '  fsolve() solves a nonlinear system of 4 equations.'

  x(1:n) = 0.0D+00

  call f3 ( n, x, fx )      
  call r8vec2_print ( n, x, fx, '  Initial X and F(X)' )

  tol = 0.00001D+00

  call fsolve ( f3, n, x, fx, tol, info )

  write ( *, '(a)' ) ' '
  if ( info == 1 ) then
    write ( *, '(a)' ) '  Satisfactory computation.'
  else
    write ( *, '(a,i6)' ) '  Returned value of INFO = ', info
  end if

  call r8vec2_print ( n, x, fx, '  Final X and F(X)' )

  return
end
subroutine f3 ( n, x, fx )

!*****************************************************************************80
!
!! f3() evaluates a nonlinear system of 4 equations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of variables.
!
!    real ( kind = 8 ) X(N), the variable values.
!
!  Output:
!
!    real ( kind = 8 ) FX(N), the function values at X.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fx(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, n
    fx(i) = ( x(i) - i )**2
  end do

  return
end
subroutine fsolve_test4 ( )

!*****************************************************************************80
!
!! fsolve_test4() tests fsolve() on a system of 8 equations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 8

  external f4
  real ( kind = 8 ) fx(n)
  integer ( kind = 4 ) info
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'fsolve_test4():'
  write ( *, '(a)' ) '  fsolve() solves a nonlinear system of 8 equations.'

  x(1:n) = 0.0D+00

  call f4 ( n, x, fx )      
  call r8vec2_print ( n, x, fx, '  Initial X and F(X)' )

  tol = 0.00001D+00

  call fsolve ( f4, n, x, fx, tol, info )

  write ( *, '(a)' ) ' '
  if ( info == 1 ) then
    write ( *, '(a)' ) '  Satisfactory computation.'
  else
    write ( *, '(a,i6)' ) '  Returned value of INFO = ', info
  end if

  call r8vec2_print ( n, x, fx, '  Final X and F(X)' )

  return
end
subroutine f4 ( n, x, fx )

!*****************************************************************************80
!
!! f4() evaluates a nonlinear system of 8 equations.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 April 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer ( kind = 4 ) N, the number of variables.
!
!    real ( kind = 8 ) X(N), the variable values.
!
!  Output:
!
!    real ( kind = 8 ) FX(N), the function values at X.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) fx(n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) x(n)

  do i = 1, n

    fx(i) = ( 3.0D+00 - 2.0D+00 * x(i) ) * x(i) + 1.0D+00

    if ( 1 < i ) then
      fx(i) = fx(i) - x(i-1)
    end if

    if ( i < n ) then
      fx(i) = fx(i) - 2.0D+00 * x(i+1)
    end if

  end do

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
subroutine timestamp ( )

!*****************************************************************************80
!
!! timestamp() prints the current YMDHMS date as a time stamp.
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

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end

